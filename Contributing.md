# Contributing to Cardano Canonical Ledger State

Thank you for your interest in contributing! We welcome contributions from the community.

## Reporting Issues

- Use [GitHub Issues](../../issues) to report bugs or request features.

## Setting up the build tools

### Using nix

Make sure you have [Nix](https://nixos.org/download.html) installed with flakes support enabled.

You can enter a Nix shell with all dependencies by running:

``` sh
nix develop
```

This will set up the environment with the required GHC version and all necessary libraries and tools.

#### Nix cache (optional, but recommended)

To speed up the build process (avoid building GHC), you should use the IOHK Nix cache.

For NixOS, add the following to your `/etc/nixos/configuration.nix` or system flake configuration:

```nix
nix.settings.substituters = [
    # ... any pre-existing substituters ...
    "https://cache.iog.io"
];
nix.settings.trusted-public-keys = [
    # ... any pre-existing keys ...
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
];
```

If using `nix` on non-NixOS systems, add the following to your `/etc/nix/nix.conf`:

```conf
substituters = https://cache.iog.io <other substituters you might have>
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= <other keys you might have>
```

#### Using direnv (optional, but recommended)

If you have [direnv](https://direnv.net/)  and [nix-direnv](https://github.com/nix-community/nix-direnv) installed, you can set it up to automatically load the Nix environment when you enter the project directory.

1. Create a `.envrc` file in the project root with the following content:

   ```sh
   watch_file \
    nix/hix.nix
   use flake
   ```

2. Allow the `.envrc` file:

   ```sh
   direnv allow
   ```

3. Now, whenever you `cd` into the project directory, direnv will automatically load the Nix environment.

### Using cabal

## Building the project

``` sh
cabal build all
```

## Testing

TODO

``` sh
cabal test all
```

## Generating documentation and setting up hoogle

TODO

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
