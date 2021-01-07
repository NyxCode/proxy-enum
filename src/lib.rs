//! Emulate dynamic dispatch and ["sealed classes"](https://kotlinlang.org/docs/reference/sealed-classes.html) using a proxy enum, which defers all method calls to its variants.
//!
//! # Introduction
//! In rust, dynamic dispatch is done using trait objects (`dyn Trait`).
//! They enable us to have runtime polymorphism, a way of expressing that a type implements a
//! certain trait while ignoring its concrete implementation.
//!
//! ```
//! let animal: &dyn Animal = random_animal();
//! animal.feed(); // may print "mew", "growl" or "squeak"
//! ```
//!
//! Trait objects come with a downside though:
//! getting a concrete implementation back from a trait object (downcasting) is painfull.
//! (see [std::any::Any])
//!
//! If you know there are only a finite number of implentations to work with, an `enum` might be
//! better at expressing such a relationship:
//! ```
//! enum Animal {
//!     Cat(Cat),
//!     Lion(Lion),
//!     Mouse(Mouse)
//! }
//!
//! match random_animal() {
//!     Animal::Cat(cat) => cat.feed(),
//!     Animal::Lion(lion) => lion.feed(),
//!     Animal::Mouse(mouse) => mouse.feed()
//! }
//! ```
//! Some languages have special support for such types, like Kotlin with so called "sealed classes".
//!
//! Rust, however, does *not*.
//!
//! `proxy-enum` simplifies working with such types using procedural macros.
//!
//! # Usage
//! ```
//! #[proxy_enum::proxy(Animal)]
//! mod proxy {
//!     enum Animal {
//!         Cat(Cat),
//!         Lion(Lion),
//!         Mouse(Mouse)
//!     }
//!
//!     impl Animal {
//!         #[implement]
//!         fn feed(&self) {}
//!     }
//! }
//! ```
//! This will expand to:
//! ```
//! mod proxy {
//!     enum Animal {
//!         Cat(Cat),
//!         Lion(Lion),
//!         Mouse(Mouse)
//!     }
//!
//!     impl Animal {
//!         fn feed(&self) {
//!             match self {
//!                 Animal::Cat(cat) => cat.feed(),
//!                 Animal::Lion(lion) => lion.feed(),
//!                 Animal::Mouse(mouse) => mouse.feed()
//!             }
//!         }
//!     }
//!     
//!     impl From<Cat> for Animal {
//!         fn from(from: Cat) -> Self {
//!             Animal::Cat(from)
//!         }
//!     }
//!
//!     impl From<Lion> for Animal {
//!         fn from(from: Lion) -> Self {
//!             Animal::Lion(from)
//!         }
//!     }
//!
//!     impl From<Mouse> for Animal {
//!         fn from(from: Mouse) -> Self {
//!             Animal::Mouse(from)
//!         }
//!     }
//! }
//! ```
//! This, however, will only compile if `Cat`, `Lion` and `Mouse` all have a method called `feed`.
//! Since rust has traits to express common functionality, trait implentations can be generated too:
//! ```
//! #[proxy_enum::proxy(Animal)]
//! mod proxy {
//!     enum Animal {
//!         Cat(Cat),
//!         Lion(Lion),
//!         Mouse(Mouse)
//!     }
//!
//!     trait Eat {
//!         fn feed(&self);
//!     }
//!
//!     #[implement]
//!     impl Eat for Animal {}
//! }
//! ```
//! Since the macro has to know which methods the trait contains, it has to be defined within the
//! module. However, implementations for external traits can be generated too:
//!
//! ```
//! #[proxy_enum::proxy(Animal)]
//! mod proxy {
//!     enum Animal {
//!         Cat(Cat),
//!         Lion(Lion),
//!         Mouse(Mouse)
//!     }
//!
//!     #[external(std::string::ToString)]
//!     trait ToString {
//!         fn to_string(&self) -> String;
//!     }
//!
//!     #[implement]
//!     impl std::string::ToString for Animal {}
//! }
//! ```

extern crate proc_macro2;

use proc_macro::TokenStream;
use std::collections::HashMap;

use proc_macro2::TokenStream as TokenStream2;
use syn::visit_mut::VisitMut;
use syn::{
    parse2, parse_macro_input, Attribute, Fields, FnArg, Ident, ImplItem, Item, ItemEnum, ItemImpl,
    ItemMod, ItemTrait, Pat, PatType, Path, PathArguments, Signature, TraitItem, Type, Variant,
};

use quote::quote;

const IMPL_ATTR: &str = "implement";
const EXT_ATTR: &str = "external";

fn attr_idx(attrs: &[Attribute], ident: &str) -> Option<usize> {
    (0..attrs.len()).find(|idx| attrs[*idx].path.is_ident(ident))
}

fn pop_attr(attrs: &mut Vec<Attribute>, ident: &str) -> Option<Attribute> {
    attr_idx(attrs, ident).map(|idx| attrs.remove(idx))
}

fn find_attr<'a>(attrs: &'a [Attribute], ident: &str) -> Option<&'a Attribute> {
    attr_idx(&attrs, ident).map(|idx| &attrs[idx])
}

fn gen_static_method_call(receiver: TokenStream2, signature: &Signature) -> TokenStream2 {
    let method_ident = &signature.ident;

    let args = signature
        .inputs
        .iter()
        .skip(1) // `self`
        .map(|a| match a {
            FnArg::Typed(PatType { pat, .. }) => match &**pat {
                Pat::Ident(ident) => &ident.ident,
                other => panic!("unsupported pattern in parameter: `{}`", quote! { #other }),
            },
            _ => panic!("parameter binding must be an identifier"),
        });

    quote! { #receiver::#method_ident(__self #(, #args)*) }
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
        .map(|variant| {
            let action = action(&variant);
            let ident = &variant.variant.ident;
            quote! { Self::#ident(__self) => #action }
        })
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
            FnArg::Typed(PatType { pat, .. }) => match &**pat {
                Pat::Ident(ident) => &ident.ident.to_string() == "self",
                _ => false,
            },
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

    let trait_ident = &trait_decl.ident;

    let proxy_methods = trait_decl.items.iter().map(|i| match i {
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

            let match_block = gen_match_block(variants, |_| gen_static_method_call(quote! { #trait_ident }, sig));
            let tokens = quote! { #sig { #match_block } };
            parse2::<ImplItem>(tokens).unwrap()
        }
        _ => panic!(
            "impl block annotated with `#[{}]` may only contain methods",
            IMPL_ATTR
        ),
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

            let match_block = gen_match_block(variants, |variant| {
                let ty = &variant.wrapped;
                gen_static_method_call(quote! { #ty }, &method.sig)
            });
            let body = quote! { { #match_block } };
            method.block = syn::parse2(body).unwrap();
        });
}

struct GenerateProxyImpl {
    proxy_enum: Ident,
    variants: Option<Vec<WrapperVariant>>,
    trait_defs: HashMap<String, ItemTrait>,
}

impl GenerateProxyImpl {
    fn new(proxy_enum: Ident) -> Self {
        GenerateProxyImpl {
            proxy_enum,
            variants: None,
            trait_defs: HashMap::new(),
        }
    }

    fn get_variants(&self) -> &[WrapperVariant] {
        self.variants
            .as_ref()
            .unwrap_or_else(|| panic!("proxy enum must be defined first"))
            .as_slice()
    }

    fn store_trait_decl(&mut self, attr: Option<Path>, decl: ItemTrait) {
        let mut path = match attr {
            Some(path) => quote! { #path },
            None => {
                let ident = &decl.ident;
                quote! { #ident }
            }
        }
        .to_string();
        path.retain(|c| !c.is_whitespace());
        self.trait_defs.insert(path, decl);
    }

    fn get_trait_decl(&self, mut path: Path) -> &ItemTrait {
        path.segments
            .iter_mut()
            .for_each(|seg| seg.arguments = PathArguments::None);
        let mut path = quote! { #path }.to_string();
        path.retain(|c| !c.is_whitespace());

        self.trait_defs
            .get(&path)
            .unwrap_or_else(|| panic!("missing declaration of trait `{}`", path))
    }
    
    fn impl_from_variants(&self, module: &mut ItemMod) {
        let proxy_enum = &self.proxy_enum;
        for WrapperVariant { variant, wrapped, .. } in self.get_variants() {
            let variant = &variant.ident;
            let tokens = quote! {
                impl From<#wrapped> for #proxy_enum {
                    fn from(from: #wrapped) -> Self {
                        #proxy_enum :: #variant(from)
                    }
                }
            };
            let from_impl: ItemImpl = syn::parse2(tokens).unwrap();
            module.content.as_mut().unwrap().1.push(from_impl.into()); 
        }
    }
}

impl VisitMut for GenerateProxyImpl {
    // store variants of our enum
    fn visit_item_enum_mut(&mut self, i: &mut ItemEnum) {
        if i.ident != self.proxy_enum {
            return;
        }
        assert!(self.variants.is_none());

        self.variants = Some(
            i.variants
                .iter()
                .cloned()
                .map(WrapperVariant::from)
                .collect(),
        );
    }

    fn visit_item_impl_mut(&mut self, impl_block: &mut ItemImpl) {
        match impl_block.trait_.as_mut() {
            // `impl Type { #[implement] fn abc() {} }
            None => implement_raw(self.get_variants(), impl_block),
            // #[implement] `impl Trait for Type {}`
            Some((_, path, _)) => {
                if pop_attr(&mut impl_block.attrs, IMPL_ATTR).is_some() {
                    implement_trait(
                        self.get_trait_decl(path.clone()),
                        self.get_variants(),
                        impl_block,
                    );
                }
            }
        };
    }

    fn visit_item_mod_mut(&mut self, module: &mut ItemMod) {
        syn::visit_mut::visit_item_mod_mut(self, module);
        // remove all items annotated with external
        module.content.as_mut().unwrap().1.retain(|item| {
            if let Item::Trait(ItemTrait { attrs, .. }) = item {
                find_attr(&attrs, EXT_ATTR).is_none()
            } else {
                true
            }
        });
        self.impl_from_variants(module);
    }

    // scan for trait declarations and store them
    fn visit_item_trait_mut(&mut self, trait_def: &mut ItemTrait) {
        let ext_attr = find_attr(&trait_def.attrs, EXT_ATTR).map(|attr| attr.parse_args().unwrap());
        self.store_trait_decl(ext_attr, trait_def.clone());
    }
}

#[proc_macro_attribute]
pub fn proxy(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut module = parse_macro_input!(item as ItemMod);
    let proxy_enum = parse_macro_input!(attr as Ident);

    GenerateProxyImpl::new(proxy_enum).visit_item_mod_mut(&mut module);

    TokenStream::from(quote! { #module })
}
