extern crate proc_macro;

use proc_macro2::{TokenStream, TokenTree, Group, Literal};
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, braced, Result, Token, Ident};
use quote::{quote, ToTokens, TokenStreamExt};
use std::ops::Range;
use syn::*;

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as Seq);

    let token_streams = input.expand();
    // let token_streams = input.expand();
    let expanded = quote! {
        #(#token_streams)*
    };

    // eprintln!("----------TOKENS DEBUG----------");
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
    fn replace_ident_if_matched(&self, stream: &mut TokenStream, token_tree : proc_macro2::TokenTree) {
        match token_tree {
            TokenTree::Ident(ident) if ident == *self.ident => {
                stream.append(TokenTree::Literal(Literal::i64_unsuffixed(self.idx)));
            },
            proc_macro2::TokenTree::Group(group) => {
                let dilimiter = group.delimiter();
                let mut modified_stream = TokenStream::new();
                for inner_token in group.stream().into_iter() {
                    self.replace_ident_if_matched(&mut modified_stream, inner_token);
                }
                stream.append(TokenTree::Group(Group::new(dilimiter, modified_stream)));
            },
            _ => {
                
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
