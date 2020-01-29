extern crate proc_macro;

use proc_macro2::{TokenStream,Span};
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput, Ident, Data, Fields, Field, Type, Type::{Path}, PathSegment, GenericArgument, MetaNameValue, Lit, Result, Error};
use syn::PathArguments::{AngleBracketed};

#[proc_macro_derive(Builder,attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let builder_type = add_builder_type(&input.ident, &input.data);
    let expanded = quote!{
        #builder_type
    };
    // eprintln!("TOKENS: {}", expanded);
    expanded.into()
}

enum FieldKind<'a> {
    Mandatory(&'a Ident, &'a Type, String), // String: not set error text
    Optional(&'a Ident, &'a Type), // wraps Option inner type
    Repeated(&'a Ident, &'a Type, Ident), // wraps Vec inner type, String: setter function name
}

fn parse_field_kind<'a>(field : &'a Field) -> Result<FieldKind<'a>> {
    match &field.ty {
        Path(type_path) => {

            let get_inner_field_kind = |segment : &'a PathSegment, outter : &str| {
                if segment.ident == outter {
                    if let AngleBracketed(arguments) = &segment.arguments {
                        if let Some(GenericArgument::Type(ty)) = arguments.args.first() {
                            return Some(ty);
                        }
                    }
                }
                return None;
            };

            let segments : Vec<&PathSegment>  = type_path.path.segments.iter().take(3).collect();
            match segments.as_slice() {
                [first, second, third] => { // std::option::Option
                    if first.ident == "std" 
                       && second.ident == "option" {
                        if let (Some(inner_ty), Some(ident)) = (get_inner_field_kind(third, "Option"),&field.ident) {
                            return Ok(FieldKind::Optional(ident, inner_ty));
                        }
                    }
                },
                [first] => { // Option
                    if let (Some(inner_ty), Some(ident)) = (get_inner_field_kind(first, "Option"),&field.ident) {
                        return Ok(FieldKind::Optional(ident, inner_ty));
                    }
                }
                _ => {}
            }

            if let Some(attr) = &field.attrs.iter().find(|x| x.path.is_ident("builder") ) {
                if let Ok(MetaNameValue{ path, eq_token: _, lit: Lit::Str(lit_str)}) = attr.parse_args::<MetaNameValue>() {
                    if path.is_ident("each") {
                        if let Path(outter_type) = &field.ty {
                            if let Some(segment) = outter_type.path.segments.first() {
                                if let (Some(ident), Some(inner_ty)) = (&field.ident, get_inner_field_kind(segment, "Vec")) {
                                    return Ok(FieldKind::Repeated(ident,inner_ty, Ident::new(&lit_str.value(), Span::call_site())));
                                }
                            }
                        }
                    } else {
                        return Err(Error::new(path.span(), r#"unrecognized attribute, expected `builder(each = "...")`"#));
                    }
                }
            } else {
                if let Some(ident) = &field.ident {
                    return Ok(FieldKind::Mandatory(ident, &field.ty, format!("{} must be set", ident.to_string())));
                }
            }

            return Err(Error::new(type_path.span(), "unable to derive"));
        },
        _ => {
            return Err(Error::new(field.ty.span(), "unable to derive"));
        },
    }
}

fn add_builder_type(ident : &Ident, data : &Data) -> TokenStream {
    let builder_ident = format_ident!{"{}Builder", ident};

    if let Data::Struct(data_struct) = data {
        let mut builder_mandatory_field_types : Vec<&Type> = Vec::new();
        let mut builder_mandatory_field_idents: Vec<&Ident> = Vec::new();
        let mut builder_mandatory_field_not_set_errors: Vec<String> = Vec::new();

        let mut builder_optional_field_inner_types : Vec<&Type> = Vec::new();
        let mut builder_optional_field_idents: Vec<&Ident> = Vec::new();

        let mut builder_repeated_field_inner_types : Vec<&Type> = Vec::new();
        let mut builder_repeated_field_idents: Vec<&Ident> = Vec::new();
        let mut builder_repeated_field_func_names: Vec<Ident> = Vec::new();

        let mut derive_error : Option<Error> = None;

        if let Fields::Named(named_fields) = &data_struct.fields {
            named_fields.named.iter()
                .for_each(|f|{
                    if derive_error.is_some() {
                        return;
                    }
                    match parse_field_kind(&f) {
                        Ok(field_kind) => {
                            match field_kind {
                                FieldKind::Mandatory(ident, ty, error_text) => {
                                    builder_mandatory_field_idents.push(ident);
                                    builder_mandatory_field_types.push(ty);
                                    builder_mandatory_field_not_set_errors.push(error_text);
                                },
                                FieldKind::Optional(ident, inner_ty) => {
                                    builder_optional_field_idents.push(ident);
                                    builder_optional_field_inner_types.push(inner_ty);
                                },
                                FieldKind::Repeated(ident, inner_type,func_name) => {
                                    builder_repeated_field_idents.push(ident);
                                    builder_repeated_field_inner_types.push(inner_type);
                                    builder_repeated_field_func_names.push(func_name);
                                }
                            }
                        },
                        Err(e) => {
                            derive_error.replace(e);
                        }
                    }
                });
        }

        if let Some(e) = derive_error {
            return e.to_compile_error();
        }

        let builder_fields_def = [
            quote!{#(#builder_mandatory_field_idents : std::option::Option<#builder_mandatory_field_types>,)*},
            quote!{#(#builder_optional_field_idents : std::option::Option<#builder_optional_field_inner_types>,)*},
            quote!{#(#builder_repeated_field_idents : std::vec::Vec<#builder_repeated_field_inner_types>,)*},
        ];

        let builder_create_def = [
            quote!{#(#builder_mandatory_field_idents : std::option::Option::None,)*},
            quote!{#(#builder_optional_field_idents : std::option::Option::None,)*},
            quote!{#(#builder_repeated_field_idents : std::vec::Vec::new(),)*},
        ];

        let builder_build_def = [
            quote!{#(#builder_mandatory_field_idents : self.#builder_mandatory_field_idents.clone().ok_or(#builder_mandatory_field_not_set_errors)?,)*},
            quote!{#(#builder_optional_field_idents : self.#builder_optional_field_idents.clone(),)*},
            quote!{#(#builder_repeated_field_idents : self.#builder_repeated_field_idents.clone(),)*},
        ];

        quote!{
            pub struct #builder_ident {
                #(#builder_fields_def)*
            }
    
            impl #ident {
                pub fn builder() -> #builder_ident {
                    #builder_ident { 
                        #(#builder_create_def)*
                     }
                }
            }

            impl #builder_ident {
                pub fn build(&self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                    std::result::Result::Ok(#ident{
                        #(#builder_build_def)*                        
                    })
                }

                #(pub fn #builder_mandatory_field_idents(&mut self, v : #builder_mandatory_field_types) -> &mut Self {
                    self.#builder_mandatory_field_idents = std::option::Option::Some(v);
                    self
                })*
                
                #(pub fn #builder_optional_field_idents(&mut self, v : #builder_optional_field_inner_types) -> &mut Self {
                    self.#builder_optional_field_idents = std::option::Option::Some(v);
                    self
                })*

                #(pub fn #builder_repeated_field_func_names(&mut self, v : #builder_repeated_field_inner_types) -> &mut Self {
                    self.#builder_repeated_field_idents.push(v);
                    self
                })*
            }
        }
    } else {
        quote!{
            pub struct #builder_ident;
            impl #ident {
                pub fn builder() -> #builder_ident {
                    #builder_ident
                }
            }
            impl #builder_ident {
                pub fn build(self) -> std::result::Result<#ident, Box<dyn Error>> {
                    std::result::Result::Ok(#ident)
                }
            }
        }
    }
}
