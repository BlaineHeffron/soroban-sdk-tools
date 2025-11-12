//! Implementation of the #[scerr] macro

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

pub fn scerr_impl(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    // TODO: Parse the enum
    // TODO: Check for #[scerr(root)] attribute
    // TODO: Assign unique u32 codes to variants
    // TODO: Handle #[transparent] and #[from_contract_client] attributes
    // TODO: Generate TryFromVal/IntoVal implementations
    // TODO: Generate FromContractError implementations
    // TODO: Generate Display implementation

    let _name = &input.ident;

    // Placeholder implementation
    let expanded = quote! {
        #[repr(u32)]
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        #input

        // TODO: Generated trait implementations
    };

    TokenStream::from(expanded)
}
