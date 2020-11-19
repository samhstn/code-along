# Learn Rust by Building Real Applications

[Udemy link](https://www.udemy.com/course/rust-fundamentals/).

"Rust is a modern systems programming language" - compared to C and C++.

Rust has no garbage collection - the same can't be said about js, python, go, etc.

An execellent learning resource is the Rust book: https://doc.rust-lang.org/stable/book/.

To get started we need the following tools installed:

```bash
# compiler
rustc --version

# package manager and build system
cargo --version

# toolchain installer for rust
rustup --version
```

These can all be installed by running:

```bash
brew install rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Cargo packages can be found in: https://crates.io/

Get started with:

```bash
cargo new example
cargo build
```
