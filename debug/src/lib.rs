extern crate proc_macro;

use std::borrow::Cow;
use std::collections::{HashSet};
use proc_macro2::{TokenStream};
use syn::{parse_macro_input, parse_quote, DeriveInput, Data, Path, LitStr, Type, Field, Ident,TypePath, PathSegment, PathArguments, DataStruct, Fields, FieldsNamed, Attribute, Meta, MetaNameValue, Lit, Generics, WherePredicate, PredicateType};
use quote::{quote};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let token_stream = add_debug_impl(input);
    eprintln!("TOKENS: {}", token_stream);

    token_stream.into()
}

fn extract_debug_attributes(attrs : &Vec<Attribute>, name : &str) -> Vec<LitStr> {
    
    attrs.iter().filter_map(|attr| {
        if attr.path.is_ident("debug") {
            if let Ok(Meta::NameValue(MetaNameValue{path, lit: Lit::Str(lit), ..})) = attr.parse_args() {
                if path.is_ident(name) {
                    return Some(lit);
                }
            }
        }

        return None;
    }).collect()
}

fn get_field_call(field : &Field) -> Option<TokenStream> {
    if let Some(ident) = &field.ident {
        let token = if let Some(custom_formatter) = extract_debug_attributes(&field.attrs, "format").pop() {
            quote!{
                .field(stringify!(#ident), &format_args!(#custom_formatter, self.#ident))
            }
        } else {
            quote!{
                .field(stringify!(#ident), &self.#ident)
            }
        };

        return Some(token);
    }

    return None;
}

struct SpecialParams<'a> {
    should_skip : HashSet<Cow<'a, Ident>>,
    where_predicates : Vec<WherePredicate>,
}

fn where_predicate_from_lit(lit : &LitStr) -> Option<(Ident,WherePredicate)> {
    if let Ok(predicate) = lit.parse() {
        if let WherePredicate::Type(PredicateType{bounded_ty:Type::Path(TypePath{path,..}),..}) = &predicate {
            if let Some(ident) = first_path_segment(&path) {
                return Some((ident.clone(), predicate));
            }
        }
    }

    return None;
}

fn first_path_segment(path : &Path) -> Option<&Ident> {
    if path.segments.len() == 1 {
            return path.get_ident();
        } else {
            if let Some(segment) = path.segments.first() {
                return Some(&segment.ident);
            };
        }
    return None;
}

fn list_special_types<'a>(fields : &'a FieldsNamed, type_param_names : HashSet<&Ident>) -> SpecialParams<'a> {
    let mut special_types = SpecialParams{should_skip : HashSet::new(), where_predicates : Vec::new()};

    let iter_phantomed_params = |path_args: &'a PathArguments| {
        if let PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{args,..}) = path_args {
            let iter = args.iter().filter_map(|arg|{
                if let syn::GenericArgument::Type(Type::Path(TypePath{path,..})) = arg {
                    return first_path_segment(path);
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
                        if let Some(ident) = first_path_segment(path) {
                            if type_param_names.contains(&ident) {
                                return Some((ident, path));
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
                               idents.for_each(|i| {special_types.should_skip.insert(Cow::Borrowed(i));});
                           }
                    }
                },
                [first] => {
                        if first.ident == "PhantomData" {
                            if let Some(idents) = iter_phantomed_params(&first.arguments) {
                                idents.for_each(|i| {special_types.should_skip.insert(Cow::Borrowed(i));});
                            }
                        } else {
                            let attributed_bounds = extract_debug_attributes(&field.attrs, "bound");
                            if attributed_bounds.len() > 0 {
                                for bound in attributed_bounds.iter() {
                                    eprintln!("bound: {}", bound.value());
                                    if let Some((ident, predicate)) = where_predicate_from_lit(bound) {
                                        special_types.should_skip.insert(Cow::Owned(ident));
                                        special_types.where_predicates.push(predicate);
                                    }
                                }
                            } else {                           
                                if let Some(idents_paths) = iter_associated_params(&first.arguments) {
                                    for (ident, path) in idents_paths {
                                        let ty = quote!{#path};
                                        special_types.should_skip.insert(Cow::Borrowed(ident));
                                        special_types.where_predicates.push(parse_quote!{#ty : std::fmt::Debug});
                                    }
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

fn append_trait_bounds(generics : &mut Generics, struct_bound_attributes: Vec<LitStr>, fields : &FieldsNamed) {
    let mut special_types = {
        let param_names : HashSet<&Ident> = generics.type_params()
            .map(|type_param| &type_param.ident)
            .collect();
        
        list_special_types(fields, param_names)
    };

    for bound in struct_bound_attributes.iter() {
        if let Some((ident, predicate)) = where_predicate_from_lit(bound) {
            special_types.should_skip.insert(Cow::Owned(ident));
            special_types.where_predicates.push(predicate);
        }
    }
    
    for type_param in generics.type_params_mut() {
        if !special_types.should_skip.contains(&type_param.ident) {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }

    if special_types.where_predicates.len() > 0 {
        let where_clause = generics.make_where_clause();
        for predicate in special_types.where_predicates {
            where_clause.predicates.push(predicate);
        }
    }
}

fn add_debug_impl(mut input : DeriveInput) -> TokenStream {
    
    let  mut struct_field_calls = Vec::new();
    if let Data::Struct(DataStruct{fields: Fields::Named(fields), ..}) = &input.data {
        let struct_debug_attributes = extract_debug_attributes(&input.attrs, "bound");
        append_trait_bounds(&mut input.generics, struct_debug_attributes,fields);
        for field in fields.named.iter() {
            if let Some(call) = get_field_call(field) {
                struct_field_calls.push(call);
            }
        }
    }
    
    let struct_name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    
    quote!{
        impl #impl_generics std::fmt::Debug for #struct_name #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                fmt.debug_struct(stringify!(#struct_name))
                #(#struct_field_calls)*
                .finish()
            }
        }
    }
}