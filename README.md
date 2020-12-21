# The Hagane Programming Language

Hagane is a compiled, statically-typed, functional programming language.

## Building

Compiling the Hagane compiler requires [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html).

To build the compiler, run `cargo build --release` in the project directory.

To run the compiler, either use `cargo run --release`, or use the executable at `target/release/hagane`.

## Usage
```
hagane input_file [output_file] [OPTION ...]
```
If running through `cargo`, replace `hagane` with `cargo run --release --`.

In the absence of `output_file` the executable is output to `input_file` + `.out`.

The following options are available:
- `-On`, where `n` is between 0 and 3 – Selects the optimization level. `-O0` is the default.
- `-print-ir` – print the LLVM IR.
- `-print-ir-unopt` – print the LLVM IR before optimization.

## Documentation

The language reference is available in `docs/langref.md`.
