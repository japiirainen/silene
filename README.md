## silene

Haskell implementation for elaborating a dependently typed language.

# Rationale

This is a place for me to implement and try out new (for me) techniques for implementing
elaboration for dependent type systems.
I am also dreaming of making the implementation as accessible as possible to other people who
interested in this topic. I got this idea from the [Grace](http:https://github.com/Gabriella439/grace) project.

In practice this means commenting the code heavily and keeping the structure as simple as
possible, thus I encourange to browse the code.

## Usage of silene

Run the `silene` parser through a file and show the output.

```sh
cabal run silene -- parse <filename>
```

## TODO for initial feature set

- [x] Lexing
- [x] Parsing
- [ ] Set up golden tests
- [ ] Set up CI
- [ ] Minimal dependent typechecking
- [ ] Evaluating to normal-form
- [ ] REPL
- [ ] proper CLI using optparse-applicative
