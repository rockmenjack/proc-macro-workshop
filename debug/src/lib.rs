extern crate proc_macro;

use std::collections::HashSet;
use proc_macro2::{TokenStream};
use syn::{parse_macro_input, parse_quote, DeriveInput, Data, Path, Type, Ident,TypePath, PathSegment, PathArguments, DataStruct, Fields, FieldsNamed, Attribute, Meta, MetaNameValue, Lit, Generics};
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

struct SpecialParams<'a> {
    skipped_params : HashSet<&'a Ident>,
    associated : Vec<&'a Path>,
}

fn list_special_types<'a>(fields : &'a FieldsNamed, type_param_names : HashSet<&Ident>) -> SpecialParams<'a> {
    let mut special_types = SpecialParams{skipped_params : HashSet::new(), associated : Vec::new()};

    let iter_phantomed_params = |path_args: &'a PathArguments| {
        if let PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{args,..}) = path_args {
            let iter = args.iter().filter_map(|arg|{
                if let syn::GenericArgument::Type(Type::Path(TypePath{path,..})) = arg {
                    if let Some(ident) = path.get_ident() {
                        return Some(ident);
                    }
                }
                return None;
            });
            
            return Some(iter);
        } else { None }
    };

    let iter_associated_params = |path_args: &'a PathArguments| {
        if let PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{args,..}) = path_args {
            let iter = args.iter().filter_map(|arg|{
                if let syn::GenericArgument::Type(Type::Path(TypePath{path,..})) = &arg {
                    if path.segments.len() > 1 {
                        if let Some(path_seg) = path.segments.first() {
                            if type_param_names.contains(&path_seg.ident) {
                                return Some((&path_seg.ident, path));
                            }
                        }
                    }
                }
                return None;
            });
            
            return Some(iter);
        } else { None }
    };


    for field in fields.named.iter() {
        if let Type::Path(type_path) = &field.ty {
            match type_path.path.segments.iter().collect::<Vec<&PathSegment>>().as_slice() {
                [first, second, third] => {
                    if (first.ident == "std" || first.ident == "core")
                       && second.ident == "marker"
                       && third.ident == "PhantomData" {
                           if let Some(idents) = iter_phantomed_params(&third.arguments) {
                               idents.for_each(|i| {special_types.skipped_params.insert(i);});
                           }
                    }
                },
                [first] => {
                    eprintln!("checking: {}", quote!{#first});
                        if first.ident == "PhantomData" {
                            if let Some(idents) = iter_phantomed_params(&first.arguments) {
                                idents.for_each(|i| {special_types.skipped_params.insert(i);});
                            }
                        } else {
                            if let Some(idents_paths) = iter_associated_params(&first.arguments) {
                                for (ident, path) in idents_paths {
                                    special_types.skipped_params.insert(ident);
                                    special_types.associated.push(path);
                                }
                            }
                        }
                },
                _ => {},
            }
        }
    }

    return special_types;
}

fn append_trait_bounds(generics : &mut Generics, fields : &FieldsNamed) {
    let special_types = {
        let param_names : HashSet<&Ident> = generics.type_params()
            .map(|type_param| &type_param.ident)
            .collect();
        
        list_special_types(fields, param_names)
    };
    
    
    for type_param in generics.type_params_mut() {
        if !special_types.skipped_params.contains(&type_param.ident) {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }

    if special_types.associated.len() > 0 {
        let where_clause = generics.make_where_clause();
        for ty in special_types.associated {
            let token = quote!{#ty};
            where_clause.predicates.push(parse_quote!{#token : std::fmt::Debug});
        }
    }
}

fn add_debug_impl(mut input : DeriveInput) -> TokenStream {
    let struct_name = &input.ident;

    let struct_fields_format;
    if let Data::Struct(DataStruct{fields: Fields::Named(fields), ..}) = &input.data {
        struct_fields_format = fields_debug_definition(fields);
        append_trait_bounds(&mut input.generics, fields);
    } else {
        struct_fields_format = Vec::new();
    }
    
    
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    
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