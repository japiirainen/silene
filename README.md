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

## Nix support

You can alternatively use nix for dev environment and for building the project.

Build:

```sh
nix build .
```

Run:

```sh
nix run .
```

Start Nix shell:

```sh
nix-shell
```

## Tips

- Run `nix flake update` to update all flake inputs.
- Run `./bin/hoogle` to start Hoogle with packages in your cabal file.
- Run the application without installing: `nix run github:srid/haskell-template` (or `nix run .` from checkout)

## TODO for initial feature set

- [x] Lexing
- [x] Parsing
- [ ] Set up golden tests
- [x] Set up CI
- [ ] Nix support
- [ ] Minimal dependent typechecking
- [ ] Evaluating to normal-form
- [ ] REPL
- [ ] proper CLI using optparse-applicative
