extern crate proc_macro;

use proc_macro2::{TokenStream};
use syn::{parse_macro_input, DeriveInput, Data, DataStruct, Fields,Attribute, Meta, MetaNameValue, Lit};
use quote::{quote};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let token_stream = add_debug_impl(input);
    eprintln!("TOKENS: {}", token_stream);

    token_stream.into()
}

fn fields_debug_definition(data : &Data) -> Vec<TokenStream> {
    if let Data::Struct(DataStruct{fields: Fields::Named(fields), ..}) = data {
        let extract_format = |attrs : &Vec<Attribute>| {
            attrs.iter().find_map(|attr| {
                if let Ok(Meta::NameValue(MetaNameValue{path, lit: Lit::Str(lit), ..})) = attr.parse_meta() {
                    if path.is_ident("debug") {
                        return Some(lit.value())
                    }
                }
                None
            })
        };

        return fields.named.iter()
            .filter_map(|f| {
                if let Some(ident) = &f.ident {
                    let tokens = if let Some(custom_formatter) = extract_format(&f.attrs) {
                        quote!{
                            .field(stringify!(#ident), &format_args!(#custom_formatter, self.#ident))
                        }
                    } else {
                        quote!{
                            .field(stringify!(#ident), &self.#ident)
                        }
                    };

                    Some(tokens)
                } else {
                    None
                }
            }).collect();
    }

    Vec::new()
}

fn add_debug_impl(input : DeriveInput) -> TokenStream {
    let struct_name = &input.ident;
    let struct_fields_format = fields_debug_definition(&input.data);
    
    quote!{
        impl std::fmt::Debug for #struct_name {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                fmt.debug_struct(stringify!(#struct_name))
                #(#struct_fields_format)*
                .finish()
            }
        }
    }
}