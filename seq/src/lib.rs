extern crate proc_macro;

use proc_macro2::{TokenStream, TokenTree, Group, Literal};
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, braced, Result, Token, Ident};
use quote::{quote, format_ident, ToTokens, TokenStreamExt};
use std::ops::Range;
use syn::*;

use core::cell::RefCell;


#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as Seq);

    // eprintln!("----------TOKENS DEBUG----------");

    let token_streams = input.expand();
    let expanded = quote! {
        #(#token_streams)*
    };

    // eprintln!("{}", expanded);
    // eprintln!("--------------------------------");
    expanded.into()
}

// syn::Ident, Token![in], syn::LitInt, Token![..], syn::LitInt.
struct Seq {
    ident : Ident,
    start : i64,
    end : i64,
    body : TokenStream,
}

struct IterBody<'a> {
    range : Range<i64>,
    ident : &'a Ident,
    body : &'a TokenStream,
}

struct Body<'a> {
    idx : i64,
    ident : &'a Ident,
    body : &'a TokenStream,
    buffered_tokens : RefCell<[Option<TokenTree>;3]>,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        let _ : Token![in] = input.parse()?;
        let start : i64 = input.parse::<LitInt>()?.base10_parse::<i64>()?;
        let _ : Token![..] = input.parse()?;
        let end : i64 = input.parse::<LitInt>()?.base10_parse::<i64>()?;
        let content;
        braced!(content in input);

        Ok(Self {ident, start, end, body: content.parse()?})
    }
}

impl Seq {
    fn expand(&self) -> IterBody {
        IterBody {
            range: Range { start: self.start, end: self.end },
            ident : &self.ident,
            body : &self.body,
        }
    }
}

impl <'a> Body<'a> {
    fn replace_ident_if_matched(&self, stream: &mut TokenStream, token_tree : TokenTree) {
        let mut append_buffered = |buf : &mut [Option<TokenTree>;3]| {
            for maybe_token in buf.iter_mut() {
                if let Some(token) = maybe_token.take() {
                    stream.append(token);
                }
            }
        };
        
        let clear_buffered = |buf : &mut [Option<TokenTree>;3]| {
            buf[0] = None;
            buf[1] = None;
            buf[2] = None;
        };

        match token_tree {
            TokenTree::Ident(ident) => {
                let buffered = &mut *self.buffered_tokens.borrow_mut();
                if ident == *self.ident {
                    match buffered {
                        // case: # <- N
                        [Some(TokenTree::Punct(_)), None, None] => buffered[1] = Some(TokenTree::Ident(ident)),
                        // case: Prefix # <- N
                        [Some(TokenTree::Ident(_)), Some(TokenTree::Punct(_)), None] => buffered[2] = Some(TokenTree::Ident(ident)),
                        _ => {
                            append_buffered(buffered);
                            stream.append(TokenTree::Literal(Literal::i64_unsuffixed(self.idx)));
                        },
                    }
                } else {
                    match buffered {
                        // case: MaybePrefix
                        [None, None, None] => buffered[0] = Some(TokenTree::Ident(ident)),
                        // case: NotPrefix <- MaybePrefix
                        [Some(TokenTree::Ident(_)), None, None] => { 
                            if let Some(prev_ident) = buffered[0].replace(TokenTree::Ident(ident)) {
                                stream.append(prev_ident);
                            };
                        },
                        // # N # <- Suffix
                        [Some(TokenTree::Punct(_)), Some(TokenTree::Ident(_)), Some(TokenTree::Punct(_))] => {
                            let i = format_ident!("{}{}", self.idx.to_string(), ident);
                            stream.append_all(quote!{#i});
                            clear_buffered(buffered);
                        },
                        _ => append_buffered(buffered),
                    }
                }
            },
            proc_macro2::TokenTree::Group(group) => {
                append_buffered(&mut *self.buffered_tokens.borrow_mut());

                let dilimiter = group.delimiter();
                let mut modified_stream = TokenStream::new();
                for inner_token in group.stream().into_iter() {
                    self.replace_ident_if_matched(&mut modified_stream, inner_token);
                }
                stream.append(TokenTree::Group(Group::new(dilimiter, modified_stream)));
            },
            TokenTree::Punct(punct) if punct.as_char() == '#' => {
                let buffered = &mut *self.buffered_tokens.borrow_mut();
                match buffered {
                    // #
                    [None, None, None] => buffered[0] = Some(TokenTree::Punct(punct)),
                    // MaybePrefix <- #
                    [Some(TokenTree::Ident(_)), None, None] => buffered[1] = Some(TokenTree::Punct(punct)),
                    // # N <- #
                    [Some(TokenTree::Punct(_)), Some(TokenTree::Ident(_)), None] => buffered[2] = Some(TokenTree::Punct(punct)),
                    // Prefix # Ident <- #
                    [Some(TokenTree::Ident(partial)), Some(TokenTree::Punct(_)), Some(TokenTree::Ident(_))] => {
                        let i = format_ident!("{}{}", partial, self.idx.to_string());
                        stream.append_all(quote!{#i});
                        clear_buffered(buffered);
                    },
                    _ => {
                        append_buffered(buffered);
                    },
                }
            }
            _ => {
                append_buffered(&mut *self.buffered_tokens.borrow_mut());
                stream.append(token_tree)
            },
        };
    }
}

impl <'a> ToTokens for Body<'a> {
    fn to_tokens(&self, stream: &mut TokenStream) {
        for token_tree in self.body.clone().into_iter() {
            self.replace_ident_if_matched(stream, token_tree);
        }
    }
}

impl <'a> Iterator for IterBody<'a> {
    type Item = Body<'a>;

    fn next(&mut self) -> Option<Body<'a>> {
        if let Some(idx) = self.range.next() {
            return Some(Body {
                idx: idx,
                ident: self.ident,
                body : &self.body,
                buffered_tokens : RefCell::new([None, None, None]),
            });
        }

        None
    }
}


// impl <'a> Iterator for IterBody<'a> {
//     type Item = TokenStream;

//     fn next(&mut self) -> Option<TokenStream> {
//         if let Some(idx) = self.range.next() {
//             return Some(Replaced {
//                 idx: idx,
//                 ident_to_replace: self.ident_to_replace,
//                 block_stmts : &self.block_body.stmts,
//             }.as_token_stream());
//         }

//         None
//     }
// }
