extern crate proc_macro;

use proc_macro2::{TokenStream};
use syn::{parse_macro_input, parse_quote, DeriveInput, Data, DataStruct, Fields,Attribute, Meta, MetaNameValue, Lit, Generics, GenericParam};
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

fn append_trait_bounds(mut generics : Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

fn add_debug_impl(input : DeriveInput) -> TokenStream {
    let struct_name = &input.ident;
    let struct_fields_format = fields_debug_definition(&input.data);
    let generics = append_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    
    quote!{
        impl #impl_generics std::fmt::Debug for #struct_name #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                fmt.debug_struct(stringify!(#struct_name))
                #(#struct_fields_format)*
                .finish()
            }
        }
    }
}