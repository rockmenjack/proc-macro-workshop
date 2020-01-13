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
            if type_path.path.segments.len() == 3 {
                let segments : Vec<_>  = type_path.path.segments.iter().take(3).collect();
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
                    _ => return None,
                }
            }
            return None;
        },
        Verbatim(_) => {},
        _ => {
            if let Some(ident) = &field.ident {
                let err_text = format!("{} is required", ident.to_string());
                return Some(FieldKind::Mandatory(ident, &field.ty, err_text));
            }
        },
    }

    return None;
}

fn add_builder_type(ident : &Ident, data : &Data) -> TokenStream {
    let builder_ident = format_ident!{"{}Builder", ident};

    if let Data::Struct(data_struct) = data {
        let mut builder_field_types : Vec<&Type> = Vec::new();
        let mut builder_field_idents: Vec<&Ident> = Vec::new();
        let mut builder_field_not_set_errors: Vec<String> = Vec::new();

        if let Fields::Named(named_fields) = &data_struct.fields {
            named_fields.named.iter()
                .for_each(|f|{
                    if let Some(ident) = &f.ident {
                        builder_field_idents.push(ident);
                        builder_field_types.push(&f.ty);
                        builder_field_not_set_errors.push(format!("{} is required", ident.to_string()));
                    }
                });
        }

        quote!{
            pub struct #builder_ident {
                #(#builder_field_idents : Option<#builder_field_types>,)*
            }
    
            impl #ident {
                pub fn builder() -> #builder_ident {
                    #builder_ident { #(#builder_field_idents : None),* }
                }
            }

            impl #builder_ident {
                #(pub fn #builder_field_idents(&mut self, v : #builder_field_types) -> &mut Self {
                    self.#builder_field_idents = Some(v);
                    self
                })*

                pub fn build(&self) -> Result<#ident, Box<dyn std::error::Error>> {
                    Ok(#ident{
                        #(#builder_field_idents : self.#builder_field_idents.clone().ok_or(String::from(#builder_field_not_set_errors))?),*
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