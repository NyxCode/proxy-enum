#![feature(box_patterns)]
extern crate proc_macro2;

use proc_macro::{Ident, TokenStream};
use std::collections::HashMap;

use syn::{Attribute, Fields, FnArg, ImplItem, Item, ItemEnum, ItemImpl, ItemMod, ItemTrait, parenthesized, parse2, parse_macro_input, Pat, Path, PathArguments, PathSegment, PatType, Signature, TraitItem, TraitItemMethod, Type, Variant, TraitItemConst};
use syn::export::TokenStream2;
use syn::fold::Fold;
use syn::parse::{Parse, ParseStream, Result as ParseResult};

use quote::quote;

const IMPL_ATTR: &str = "implement";
const EXT_ATTR: &str = "external";
const PROXY_ATTR: &str = "proxy";

fn attr_idx(attrs: &[Attribute], ident: &str) -> Option<usize> {
    (0..attrs.len())
        .into_iter()
        .find(|idx| attrs[*idx].path.is_ident(ident))
}

fn pop_attr(attrs: &mut Vec<Attribute>, ident: &str) -> Option<Attribute> {
    attr_idx(attrs, ident)
        .map(|idx| attrs.remove(idx))
}

fn find_attr<'a>(attrs: &'a [Attribute], ident: &str) -> Option<&'a Attribute> {
    attr_idx(&attrs, ident)
        .map(|idx| &attrs[idx])
}

fn gen_static_method_call(
    receiver_ty: &Type,
    signature: &Signature,
) -> TokenStream2 {
    let method_ident = &signature.ident;

    let args = signature
        .inputs
        .iter()
        .skip(1) // `self`
        .map(|a| match a {
            FnArg::Typed(PatType {
                             pat: box Pat::Ident(ident),
                             ..
                         }) => &ident.ident,
            _ => panic!("parameter binding must be an identifier"),
        });

    quote! { <#receiver_ty>::#method_ident(__self #(, #args)*) }
}

struct WrapperVariant {
    variant: Variant,
    wrapped: Type,
}

impl From<Variant> for WrapperVariant {
    fn from(variant: Variant) -> Self {
        match &variant.fields {
            Fields::Unnamed(a) if a.unnamed.len() == 1 => WrapperVariant {
                variant: variant.clone(),
                wrapped: a.unnamed.first().unwrap().ty.clone(),
            },
            _ => panic!("expected a variant with a single unnamed value"),
        }
    }
}

fn gen_match_block(
    variants: &[WrapperVariant],
    action: impl Fn(&WrapperVariant) -> TokenStream2,
) -> TokenStream2 {
    let branches = variants
        .iter()
        .map(
            |variant| {
                let action = action(&variant);
                let ident = &variant.variant.ident;
                quote! { Self::#ident(__self) => #action }
            },
        )
        .collect::<Vec<_>>();

    quote! {
        match self {
            #(#branches),*
        }
    }
}

fn has_self_param(sig: &Signature) -> bool {
    sig.inputs
        .first()
        .map(|param| match param {
            FnArg::Receiver(..) => true,
            FnArg::Typed(PatType {
                             pat: box Pat::Ident(ident),
                             ..
                         }) => &ident.ident.to_string() == "self",
            _ => false,
        })
        .unwrap_or(false)
}

/// populate an empty `#[implement] impl Trait for ProxyEnum {}` block
fn implement_trait(
    trait_decl: &ItemTrait,
    variants: &[WrapperVariant],
    pseudo_impl: &mut ItemImpl,
) {
    assert!(pseudo_impl.items.is_empty());

    let receiver = &pseudo_impl.trait_.as_ref().unwrap().1;
    let trait_ty = parse2::<Type>(quote! { #receiver }).unwrap();

    let proxy_methods = trait_decl
        .items
        .iter()
        .map(|i| match i {
            TraitItem::Method(i) => {
                let sig = &i.sig;
                if !has_self_param(sig) {
                    match &i.default {
                        Some(..) => return parse2(quote! { #i }).unwrap(),
                        None => panic!(
                            "`{}` has no self parameter or default implementation",
                            quote! { #sig }
                        ),
                    }
                }

                let match_block = gen_match_block(
                    variants,
                    |variant| gen_static_method_call(&trait_ty, sig)
                );
                let tokens = quote! { #sig { #match_block } };
                parse2::<ImplItem>(tokens).unwrap()
            },
            other => panic!("impl block annotated with `#[{}]` may only contain methods", IMPL_ATTR),
        });

    pseudo_impl.items = proxy_methods.collect();
}

/// populate methods in a `impl ProxyEnum { #[implement] fn method(&self) {} }` block
fn implement_raw(variants: &[WrapperVariant], pseudo_impl: &mut ItemImpl) {
    pseudo_impl
        .items
        .iter_mut()
        .flat_map(|i| match i {
            ImplItem::Method(method) => pop_attr(&mut method.attrs, IMPL_ATTR).map(|_| method),
            _ => None,
        })
        .for_each(|mut method| {
            if !method.block.stmts.is_empty() {
                panic!("method annotated with `#[{}]` must be empty", IMPL_ATTR)
            }


            let match_block = gen_match_block(
                variants,
                |variant| gen_static_method_call(&variant.wrapped, &method.sig)
            );
            let body = quote! { { #match_block } };
            method.block = syn::parse2(body).unwrap();
        });
}

struct GenerateProxyImpl {
    variants: Option<Vec<WrapperVariant>>,
    trait_declarations: HashMap<String, ItemTrait>,
}

impl GenerateProxyImpl {
    fn get_variants(&self) -> &[WrapperVariant] {
        self
            .variants
            .as_ref()
            .unwrap_or_else(|| panic!("`#[{}]` enum must be defined first", PROXY_ATTR))
            .as_slice()
    }

    fn store_trait_decl(&mut self, attr: Option<Path>, decl: ItemTrait) {
        let mut path = match attr {
            Some(path) => quote! { #path },
            None => {
                let ident = &decl.ident;
                quote! { #ident }
            }
        }.to_string();
        path.retain(|c| !c.is_whitespace());
        self.trait_declarations.insert(path, decl);
    }

    fn get_trait_decl(&self, mut path: Path) -> &ItemTrait {
        path.segments.iter_mut().for_each(|seg| seg.arguments = PathArguments::None);
        let mut path = quote! { #path }.to_string();
        path.retain(|c| !c.is_whitespace());

        self.trait_declarations.get(&path)
            .unwrap_or_else(|| panic!("missing declaration of trait `{}`", path))
    }
}

impl Default for GenerateProxyImpl {
    fn default() -> Self {
        GenerateProxyImpl {
            variants: None,
            trait_declarations: HashMap::new(),
        }
    }
}

impl Fold for GenerateProxyImpl {
    // store variants of our enum
    fn fold_item_enum(&mut self, mut i: ItemEnum) -> ItemEnum {
        if pop_attr(&mut i.attrs, PROXY_ATTR).is_some() {
            if self.variants.is_some() {
                panic!("only one enum can be annotated with `#[{}]`", PROXY_ATTR)
            }

            self.variants = Some(
                i.variants
                    .iter()
                    .cloned()
                    .map(WrapperVariant::from)
                    .collect(),
            );
        }
        i
    }

    fn fold_item_impl(&mut self, mut i: ItemImpl) -> ItemImpl {
        match i.trait_.as_mut() {
            // `impl Type { #[implement] fn abc() {} }
            None => implement_raw(self.get_variants(), &mut i),
            // #[implement] `impl Trait for Type {}`
            Some((_, path, _)) => {
                if pop_attr(&mut i.attrs, IMPL_ATTR).is_none() {
                    return i;
                }

                implement_trait(
                    self.get_trait_decl(path.clone()),
                    self.get_variants(),
                    &mut i,
                );
            }
        };
        i
    }

    fn fold_item_mod(&mut self, i: ItemMod) -> ItemMod {
        let mut folded = syn::fold::fold_item_mod(self, i);
        // remove all items annotated with external
        folded.content.as_mut().unwrap().1.retain(|item| {
            if let Item::Trait(ItemTrait { attrs, .. }) = item {
                find_attr(&attrs, EXT_ATTR).is_none()
            } else {
                true
            }
        });
        folded
    }

    // scan for trait declarations and store them
    fn fold_item_trait(&mut self, i: ItemTrait) -> ItemTrait {
        let ext_attr = find_attr(&i.attrs, EXT_ATTR)
            .map(|attr| attr.parse_args().unwrap());
        self.store_trait_decl(ext_attr, i.clone());
        i
    }
}

#[proc_macro_attribute]
pub fn module(_: TokenStream, item: TokenStream) -> TokenStream {
    let mut module = parse_macro_input!(item as ItemMod);

    module = GenerateProxyImpl::default()
        .fold_item_mod(module);

    TokenStream::from(quote! { #module })
}
