# proxy-enum
![Crates.io](https://img.shields.io/crates/d/proxy-enum?style=flat-square)
![](https://docs.rs/mio/badge.svg)  

Emulate dynamic dispatch and ["sealed classes"](https://kotlinlang.org/docs/reference/sealed-classes.html) using a proxy enum, which defers all method calls to its variants.

## Introduction
In rust, dynamic dispatch is done using trait objects (`dyn Trait`).
They enable us to have runtime polymorphism, a way of expressing that a type implements a
certain trait while ignoring its concrete implementation.

```rust
let animal: &dyn Animal = random_animal();
animal.feed(); // may print "mew", "growl" or "squeak"
```

Trait objects come with a downside though:
getting a concrete implementation back from a trait object (downcasting) is painfull.
(see [std::any::Any])

If you know there are only a finite number of implentations to work with, an `enum` might be
better at expressing such a relationship:
```rust
enum Animal {
    Cat(Cat),
    Lion(Lion),
    Mouse(Mouse)
}

match random_animal() {
    Animal::Cat(cat) => cat.feed(),
    Animal::Lion(lion) => lion.feed(),
    Animal::Mouse(mouse) => mouse.feed()
}
```
Some languages have special support for such types, like Kotlin with so called "sealed classes".

Rust, however, does *not*.

`proxy-enum` simplifies working with such types using procedural macros.

## Usage
```rust
#[proxy_enum::proxy(Animal)]
mod proxy {
    enum Animal {
        Cat(Cat),
        Lion(Lion),
        Mouse(Mouse)
    }

    impl Animal {
        #[implement]
        fn feed(&self) {}
    }
}
```
This will expand to:
```rust
mod proxy {
    enum Animal {
        Cat(Cat),
        Lion(Lion),
        Mouse(Mouse)
    }

    impl Animal {
        fn feed(&self) {
            match self {
                Animal::Cat(cat) => cat.feed(),
                Animal::Lion(lion) => lion.feed(),
                Animal::Mouse(mouse) => mouse.feed()
            }
        }
    }
}
```
This, however, will only compile if `Cat`, `Lion` and `Mouse` all have a method called `feed`.
Since rust has traits to express common functionality, trait implentations can be generated too:
```rust
#[proxy_enum::proxy(Animal)]
mod proxy {
    enum Animal {
        Cat(Cat),
        Lion(Lion),
        Mouse(Mouse)
    }

    trait Eat {
        fn feed(&self);
    }

    #[implement]
    impl Eat for Animal {}
}
```
Since the macro has to know which methods the trait contains, it has to be defined within the
module. However, implementations for external traits can be generated too:

```rust
#[proxy_enum::proxy(Animal)]
mod proxy {
    enum Animal {
        Cat(Cat),
        Lion(Lion),
        Mouse(Mouse)
    }

    #[external(std::string::ToString)]
    trait ToString {
        fn to_string(&self) -> String;
    }

    #[implement]
    impl std::string::ToString for Animal {}
}
```