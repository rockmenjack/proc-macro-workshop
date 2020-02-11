extern crate proc_macro;

use proc_macro2::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Result, Token, Ident, LitInt};
use quote::{quote};

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let _ = parse_macro_input!(input as Seq);

    (quote!{}).into()
}

// syn::Ident, Token![in], syn::LitInt, Token![..], syn::LitInt.
struct Seq {
    ident : Ident,
    in_token : Token![in],
    start : LitInt,
    dot_dot : Token![..],
    end : LitInt,
    remainings : TokenStream,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            ident : input.parse()?,
            in_token : input.parse()?,
            start : input.parse()?,
            dot_dot : input.parse()?,
            end : input.parse()?,
            remainings : input.parse()?,
        })
    }
}