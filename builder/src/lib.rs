extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{format_ident, quote,quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput, Ident, Data, Fields, Field, Type, Type::{Path, Verbatim}, GenericArgument};
use syn::PathArguments::{AngleBracketed};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let builder_type = add_builder_type(&input.ident, &input.data);
    let expanded = quote!{
        #builder_type
    };
    eprintln!("TOKENS: {}", expanded);
    expanded.into()
}

enum FieldKind<'a> {
    Mandatory(&'a Ident, &'a Type, String),
    Optional(&'a Ident, &'a Type), // wraps inner type
}

fn parse_field_kind(field : &Field) -> Option<FieldKind> {
    match &field.ty {
        Path(type_path) => {
            let segments : Vec<_>  = type_path.path.segments.iter().take(3).collect();
            eprintln!("!!!: {}", quote!{#(#segments)*});
            match segments.as_slice() {
                [first, second, third] => {
                    if first.ident.to_string() == "std" 
                        && second.ident.to_string() == "option"
                        && third.ident.to_string() == "Option" {
                        if let AngleBracketed(arguments) = &third.arguments {
                            if let Some(GenericArgument::Type(ty)) = arguments.args.first() {
                                
                                if let Some(ident) = &field.ident {
                                    return Some(FieldKind::Optional(ident, ty))
                                }
                            }
                        }
                    }
                    return None;
                },
                _ => {
                    if let Some(ident) = &field.ident {
                        if ident.to_string() == "Option" {
                            return Some()
                        } else {
                            return Some(FieldKind::Mandatory(ident, &field.ty, format!("{} must be set", ident.to_string()));
                        }
                    }
                }
            }

            return None;
        },
        _ => {
            return None;
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

        if let Fields::Named(named_fields) = &data_struct.fields {
            named_fields.named.iter()
                .for_each(|f|{
                    if let Some(field_kind) = parse_field_kind(&f) {
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
                        }
                    }
                });
        }

        quote!{
            pub struct #builder_ident {
                #(#builder_mandatory_field_idents : Option<#builder_mandatory_field_types>,)*
                #(#builder_optional_field_idents : #builder_optional_field_inner_types,)*
            }
    
            impl #ident {
                pub fn builder() -> #builder_ident {
                    #builder_ident { 
                        #(#builder_mandatory_field_idents : None),*
                        #(#builder_optional_field_idents : None),*
                     }
                }
            }

            impl #builder_ident {
                #(pub fn #builder_mandatory_field_idents(&mut self, v : #builder_mandatory_field_types) -> &mut Self {
                    self.#builder_mandatory_field_idents = Some(v);
                    self
                })*
                
                #(pub fn #builder_optional_field_idents(&mut self, v : #builder_optional_field_inner_types) -> &mut Self {
                    self.#builder_optional_field_idents = Some(v);
                    self
                })*

                pub fn build(&self) -> Result<#ident, Box<dyn std::error::Error>> {
                    Ok(#ident{
                        #(#builder_mandatory_field_idents : self.#builder_mandatory_field_idents.clone().ok_or(#builder_mandatory_field_not_set_errors)?),*
                        #(#builder_optional_field_idents : self.#builder_optional_field_idents),*
                    })
                }
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
                pub fn build(self) -> Result<#ident, Box<dyn Error>> {
                    Ok(#ident)
                }
            }
        }
    }
}

// if let (#(Some(#builder_field_idents)),*) = (#(self.#builder_field_idents),*) {
//     Ok(#ident{
//         #(#builder_field_idents : #builder_field_idents,)*
//     })
// } else {
//     unreachable!()
// }

// Ok(#ident{
//     #(#builder_field_idents : #builder_field_idents,)*
// })

// fn add_field_setters(data : &Data) -> TokenStream {
//     if let Data::Struct(struct_data) = data {
//         if let Fields::Named(fields) = struct_data {
//             let func_defs = fields.named.iter().map(|f| {
//                 let name = &f.ident;
//                 quote!{
//                     pub fn #name(&mut self, v : f.)
                    
//                 }
//             });
//         }
//     }
// }