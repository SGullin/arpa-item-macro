//! Derive macro crate for `argos-arpa`.

#![warn(
    missing_docs,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo
)]
#![allow(clippy::must_use_candidate)]

extern crate quote;
extern crate syn;
extern crate proc_macro;

use proc_macro2::TokenStream;
use syn::{parse_macro_input, spanned::Spanned, DataStruct, DeriveInput};
use quote::quote;

type Field = (syn::Ident, Vec<syn::Attribute>, syn::Type);

/// Implements a set of functions to ease the SQL data handling. A struct using
/// `derive(CahceTableItem)` must have a field `id: i32`, and all other fields 
/// must be named and implement `sqlx::FromRow`.
/// 
/// - Use `#[unique]` for values that may not be repeated in any row.
/// - Use `#[derived]` for values that are set automatically by the DB and thus
///     not `insert`ed.
/// - Use `#[secret]` for values that should not be selected when downloading.
#[proc_macro_derive(TableItem, attributes(table, unique, derived, secret))]
pub fn item_macro(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let Some(attribute) = input.attrs.iter().find(|a| 
        a.path().segments.len() == 1 && 
        a.path().segments[0].ident == "table"
    ) else {
        return syn::Error::new(
            input.span(), 
            "`table(Table::t)` attribute required for deriving TableItem!"
        ).to_compile_error().into();
    };
    
    let Ok(table) = attribute.parse_args::<syn::Ident>() else {
        return syn::Error::new(
            input.span(), 
            "Malformed table argument!"
        ).to_compile_error().into();
    };

    let name = input.ident;
    let fields = match input.data {
        syn::Data::Struct(DataStruct { 
            fields: syn::Fields::Named (ref fields), 
            ..
        }) => {
            fields.named
            .iter()
            .filter_map(|field| {
                field.ident
                .clone()
                .map(|id| (
                    id,
                    field.attrs.clone(),
                    field.ty.clone()
                ))
            })
            .collect::<Vec<_>>()
        }

        _ => unimplemented!(),
    };

    let insert_columns = insert_columns(&fields);
    let insert_values = insert_values(&fields);
    let unique_values = unique_values(&fields);
    let select = select(&fields);
 
    let expanded = quote! {
        impl TableItem for #name {
            const TABLE: crate::Table = crate::Table::#table;

            fn id(&self) -> i32 { 
                self.id
            }
            #insert_columns
            #insert_values
            #unique_values
            #select
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn insert_columns(fields: &[Field]) -> TokenStream {
    let line = collect_named_data_inverse(
        fields, 
        Some("derived"), 
        ",", 
        |f| f.0.to_string()
    );

    quote! {
        fn insert_columns() -> &'static str {
            #line
        }
    }
}

fn insert_values(fields: &[Field]) -> TokenStream {
    let (brackets, ids): (Vec<_>, Vec<_>) = fields
        .iter()
        .filter(|(_, atts, _)| atts
            .iter()
            .all(|a| !is_attribute(a, Some("derived")))
        ).map(|(id, _, t)| match get_typetype(t) {
            TypeType::OptionStringish => (
                false,
                quote! {
                    self.#id
                        .as_ref()
                        .map_or_else(|| "NULL".into(), |v| format!("'{v}'"))
                }
            ),

            TypeType::Option => (
                false,
                quote! {
                    self.#id
                        .as_ref()
                        .map_or_else(|| "NULL".into(), |v| format!("{v}"))
                }
            ),

            TypeType::Stringish => (true, quote! {self.#id}),
            TypeType::Casual => (false, quote! {self.#id}),
        })
        .unzip();

    let line = brackets
        .into_iter()
        .map(|to_quote| if to_quote { "'{}' " } else { "{} " })
        .collect::<Vec<_>>()
        .join(",");

    quote! {
        fn insert_values(&self) -> String {
            format!(#line, #(#ids,)*)
        }
    }
}

enum TypeType {
    Casual,
    Stringish,
    Option,
    OptionStringish,
}

fn get_typetype(t: &syn::Type) -> TypeType {
    if is_stringish(t) { return TypeType::Stringish; }

    if let syn::Type::Path(path) = t {
        let option = path.path.segments
            .iter()
            .find(|seg| seg.ident.eq("Option"));

        option.map_or(TypeType::Casual, |op| match &op.arguments {
            syn::PathArguments::None => 
                unimplemented!("Empty options have no support"),

            syn::PathArguments::AngleBracketed(args) => 
                match &args.args[0] {
                // Options only have one argument
                    syn::GenericArgument::Type(t) => 
                        if is_stringish(t) { TypeType::OptionStringish }
                        else { TypeType::Option },
                    _ => unimplemented!("Only types go in options"),
                },

            syn::PathArguments::Parenthesized(_) => 
                unimplemented!("Tuples have no support"),
        })
    }
    else { TypeType::Casual }
}

fn is_stringish(t: &syn::Type) -> bool {
    match t {
        syn::Type::Path(path) => 
            path.path.segments.iter().any(|seg| 
                seg.ident.eq("String") ||
                seg.ident.eq("Uuid")
            ),

        _ => false
    }
}

fn unique_values(fields: &[Field]) -> TokenStream {
    let (brackets, ids): (Vec<_>, Vec<_>) = fields
        .iter()
        .filter(|(_, atts, _)| atts
            .iter()
            .any(|a| is_attribute(a, Some("unique")))
        ).map(|(id, _, t)| ((is_stringish(t), id.to_string()), id))
        .unzip();

    let line = brackets
        .into_iter()
        .map(|(is_str, name)| format!(
            "{}={}",
            name,
            if is_str { "'{}'" } else { "{}" }
        ))
        .collect::<Vec<_>>()
        .join(" or ");

    quote! {
        fn unique_values(&self) -> String {
            format!(#line, #(self.#ids,)*)
        }
    }
}

fn select(fields: &[Field]) -> TokenStream {
    let line = collect_named_data_inverse(
        fields, 
        Some("secret"),
        ",", 
        |f| f.0.to_string()
    );

    quote! {    
        fn select() -> &'static str {
            #line
        }
    }
}

fn collect_named_data_inverse<F>(
    fields: &[Field],
    attribute: Option<&'static str>,
    joiner: &'static str,
    treatment: F,
) -> String where F: Fn(&Field) -> String {
    fields
    .iter()
    .filter(|&field| field.1
        .iter()
        .all(|a| !is_attribute(a, attribute))
    )
    .map(treatment)
    .collect::<Vec<_>>()
    .join(joiner)
}

fn is_attribute(a: &syn::Attribute, attr: Option<&'static str>) -> bool {
    attr.is_none_or(|attr| match &a.meta {
        syn::Meta::Path(path) => path.is_ident(attr),
        _ => false,
    })
}
