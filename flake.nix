{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks = {
    url = "github:cachix/pre-commit-hooks.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.treefmt-nix = {
    url = "github:numtide/treefmt-nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    { self, nixpkgs, flake-utils, haskellNix, pre-commit-hooks, treefmt-nix }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        defaultCompiler = "ghc910";

        overlays = [
          haskellNix.overlay
          (final: _prev: {
            cardanoCanonicalLedger = final.haskell-nix.cabalProject' {
              src = ./.;

              name = "cardano-canonical-ledger";
              compiler-nix-name = final.lib.mkDefault defaultCompiler;

              # Tools to include in the development shell
              shell.tools = {
                cabal = "latest";
                haskell-language-server = "latest";
                hlint = "latest";
                fourmolu = "latest";
                weeder = "latest";
                cabal-gild = "latest";
              };

              # Non-Haskell shell tools go here
              shell.buildInputs = let
                # Add this for editors which expect to use hls-wrapper
                hls-wrapper =
                  pkgs.writeShellScriptBin "haskell-language-server-wrapper" ''
                    exec haskell-language-server "$@"
                  '';
              in with pkgs; [ nixfmt-classic hls-wrapper ];
            };
          })
        ];

        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        inherit (pkgs) lib;

        flake = pkgs.cardanoCanonicalLedger.flake
          (lib.optionalAttrs (system == "x86_64-linux") {
            # on linux, build/test other supported compilers
            variants = lib.genAttrs [ "ghc984" "ghc9102" "ghc9121" ]
              (compiler-nix-name: { inherit compiler-nix-name; });
          });
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            fourmolu.enable = true;
            fourmolu.package =
              pkgs.haskell-nix.tool "ghc910" "fourmolu" "latest";
            nixfmt-classic.enable = true;
            cabal-gild.enable = true;
            cabal-gild.package =
              pkgs.haskell-nix.tool "ghc910" "cabal-gild" "latest";
          };
        };
        treefmtEval = treefmt-nix.lib.evalModule pkgs ./nix/treefmt.nix;
      in flake // {
        legacyPackages = pkgs;
        checks = {
          pre-commit-check = pre-commit-check;
          formatting = treefmtEval.${pkgs.system}.config.build.check self;
        };
        devShells = {
          default = flake.devShells.default.overrideAttrs (oldAttrs: {
            inherit (pre-commit-check) shellHook;
            buildInputs = (oldAttrs.buildInputs or [ ])
              ++ pre-commit-check.enabledPackages;
          });
        };
        formatter = treefmtEval.config.build.wrapper;
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
