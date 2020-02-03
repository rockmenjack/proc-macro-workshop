extern crate proc_macro;

use std::collections::HashSet;
use proc_macro2::{TokenStream};
use syn::{parse_macro_input, parse_quote, DeriveInput, Data, Type, Ident, PathSegment, PathArguments, DataStruct, Fields, FieldsNamed, Attribute, Meta, MetaNameValue, Lit, Generics, GenericParam};
use quote::{quote};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let token_stream = add_debug_impl(input);
    eprintln!("TOKENS: {}", token_stream);

    token_stream.into()
}

fn fields_debug_definition(fields : &FieldsNamed) -> Vec<TokenStream> {
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
            }).collect()
}

fn list_phantomed_types<'a>(fields : &'a FieldsNamed) -> HashSet<&'a Ident> {
    let mut skip_types = HashSet::new();

    let mut extract_type = |path_args: &'a PathArguments| {
        if let PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{args,..}) = path_args {
            if let Some(syn::GenericArgument::Type(Type::Path(type_path))) = args.first() {
                if let Some(ident) = type_path.path.get_ident() {
                    skip_types.insert(ident);
                }                
            }
        }
    };

    for field in fields.named.iter() {
        if let Type::Path(type_path) = &field.ty {
            match type_path.path.segments.iter().collect::<Vec<&PathSegment>>().as_slice() {
                [first, second, third] => {
                    if (first.ident == "std" || first.ident == "core")
                       && second.ident == "marker"
                       && third.ident == "PhantomData" {
                        extract_type(&third.arguments);
                    }
                },
                [first] => {
                    if first.ident == "PhantomData" {
                        extract_type(&first.arguments);
                    }
                },
                _ => (),
            }
        }
    }

    return skip_types;
}

fn append_trait_bounds(skip_types : &Option<HashSet<&Ident>>, mut generics : Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            if let Some(skip_types) = skip_types {
                if skip_types.contains(&type_param.ident) {
                    continue;
                }
            }
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    generics
}

fn add_debug_impl(input : DeriveInput) -> TokenStream {
    let struct_name = &input.ident;

    let (struct_fields_format, skip_types) =
    if let Data::Struct(DataStruct{fields: Fields::Named(fields), ..}) = &input.data {
        (fields_debug_definition(fields), Some(list_phantomed_types(fields)))
    } else {
        (Vec::new(), None)
    };
    
    
    let generics = append_trait_bounds(&skip_types, input.generics);
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