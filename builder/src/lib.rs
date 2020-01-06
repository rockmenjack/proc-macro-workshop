extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{format_ident, quote,quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput, Ident, Data, Fields, Type};

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

fn add_builder_type(ident : &Ident, data : &Data) -> TokenStream {
    let builder_ident = format_ident!{"{}Builder", ident};

    if let Data::Struct(data_struct) = data {
        let mut builder_field_types : Vec<&Type> = Vec::new();
        let mut builder_field_idents: Vec<&Ident> = Vec::new();

        if let Fields::Named(named_fields) = &data_struct.fields {
            named_fields.named.iter()
                .for_each(|f|{
                    if let Some(ident) = &f.ident {
                        builder_field_idents.push(ident);
                        builder_field_types.push(&f.ty);   
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

                pub fn build(self) -> Result<#ident, Box<dyn Error>> {
                    #(<self.#builder_field_idents>::is_none() {
                        return format!("{} is not set", #builder_field_idents);
                    })*
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