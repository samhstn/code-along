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
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

To solve any "nightly-... is not installed" commands, run:

```
# see toolchains
rustup toolchain list

# install the nightly version of our toolchain
# for me this was:
rustup toolchain install nightly-x86_64-apple-darwin
```

Then add `$HOME/.cargo/bin` to the `$PATH`.

Cargo packages can be found in: https://crates.io/

Get started with a new project with:

```bash
cargo new example
cargo build
```

### The Stack

The stack is a special region of the process memory that stores variables created by each function.
The memory for each function is called a stack frame - this is where our local variables live.

For every function call, a new stack frame is allocated on top of the current one.

The size of every variable has to be known at compile time.
If we want to store an array on the stack, we have to specify how many elements it will hold.
When a function exits, it's stack fame is released - the memory is managed for us.

### The Heap

A region of the process memory the is NOT automatically managed for us.
Failing to release Heap allocated memory leads to memory leaks.
It has no size restrictions.
It is accessible by any function anywhere in the program.
It is expensive, so should be avoided where possible.

Each heap allocation has a memory address, this address is stored in the stack, this pointer can be referenced.

### Smart pointer

Makes sure to free Heap memory when the pointer goes out of scope.

### Data types

Rust has 4 data types:
+ Booleans
+ Characters
+ Integers
+ Floats

Integers have type types, u{8,16,32,64,128} and i{8,16,32,64,128},
an unsigned integer can only be positive and a signed integer can be positive or negative.
where u8 is an 8 bit, unsigned integer.

We have 2 types of floating point numbers, f32 and f64 where f64 has double the precision of f32.

Booleans in rust are 1 byte in size.

A char holds a single unicode value and is always 4 bytes in size.

### Language

In rust, the last expression in any function that doesn't have a semi-colon at the end is automatically returned.

A macro is different from a function in that it can be called with a variable, number of parameters and different types,
but their definitions are more complex as you are writing rust code that generates rust code.

Marcos are differentiated from functions by a trailing '!'.

To see all the code that our call is expanded in to,
install `cargo-expand` with `$ cargo install cargo-expand`.

And run it with `$ cargo expand`.

We can run our Mars Calculator with `$ cargo run`.
