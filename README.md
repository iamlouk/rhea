# Rhea Programming Language v2

- Older Version: [v1](./v1)
- LLVM Bindings: [inkwell](https://github.com/TheDan64/inkwell)
- Parser Generator: [larpop](https://github.com/lalrpop/lalrpop)
- Examples can be found in `./examples`

## Hello World

```
extern printf := (fmt: *Char, ...): Int;

main := (): Int -> {
	printf("Hello, World!\n");
	0
};
```

If this is the contents of file `hello.rhea`, one
could run it using: `cargo run -- --jit --input hello.rhea`.

### TODO
- [ ] Small Standard-Library written in Rust
- [ ] Something like Ownership
- [ ] More Custom Datatypes (Enums, ...)
- [ ] Closures/Anonnymus Functions

