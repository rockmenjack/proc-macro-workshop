extern crate proc_macro;

use proc_macro2::{TokenStream};
use syn::{parse_macro_input, DeriveInput, Ident, Data, DataStruct, Fields};
use quote::{quote};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let token_stream = add_debug_impl(input);
    eprintln!("TOKENS: {}", token_stream);

    token_stream.into()
}

fn list_struct_fields(data : &Data) -> Vec<&Ident> {
    if let Data::Struct(DataStruct{fields: Fields::Named(fields), ..}) = data {
        fields.named.iter().filter_map(|f| f.ident.as_ref()).collect()
    } else {
        Vec::new()
    }
}

fn add_debug_impl(input : DeriveInput) -> TokenStream {
    let struct_name = &input.ident;
    let struct_name_str = struct_name.to_string();
    let struct_fields = list_struct_fields(&input.data);

    let struct_fields_str = struct_fields.iter().map(|f| f.to_string());
    
    quote!{
        impl std::fmt::Debug for #struct_name {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                fmt.debug_struct(#struct_name_str)
                #(.field(#struct_fields_str, &self.#struct_fields))*
                .finish()
            }
        }
    }
}