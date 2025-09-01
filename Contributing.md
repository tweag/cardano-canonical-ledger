# Contributing to Cardano Canonical Ledger State

Thank you for your interest in contributing! We welcome contributions from the community.

## Reporting Issues

- Use [GitHub Issues](../../issues) to report bugs or request features.

## Setting up the build tools

### Using nix

TODO

### Using cabal

In order to work with the project you need to install [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/) tools, we suggest installing them using [GHCup](https://www.haskell.org/ghcup/) project. For working with
this project you need to have GHC>=9.6 and cabal>=3.10

To install ghcup follow the instructions on site. After installing run

```
ghcup tui
```

And select recommended versions of GHC an cabal.

## Building the project

To build the project in the project directory run command:

``` sh
cabal build all
```

## Testing

To run tests in the project directory run command:

``` sh
cabal test all
```

## Generating documentation and setting up hoogle

To generate documentation run

``` sh
cabal haddock all
```

## How to Contribute

1. **Fork the repository** and create your branch from `main`.
1. **Clone your fork** and set up the project locally, see setting up section.
1. **Make your changes** with clear, descriptive commit messages.
1. **Open a Pull Request** describing your changes and referencing any related issues.

## Code of conduct

Project follows the same code conventions as cardano project [Contributor Covenant][cc-homepage].

## License

By contributing, you agree that your contributions will be licensed under the project's license.

---

Thank you for helping improve Cardano Canonical Ledger!

[cc-homepage]: https://www.contributor-covenant.org