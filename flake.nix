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
        pkgs = import nixpkgs {
          overlays = [ haskellNix.overlay ];
          inherit system;
          inherit (haskellNix) config;
        };

        inherit (pkgs) lib;

        defaultCompiler = "ghc910";
        fourmoluVersion = "0.19.0.0";
        cabalGildVersion = "1.6.0.2";

        cardanoCanonicalLedger = pkgs.haskell-nix.cabalProject' {
          src = ./.;

          name = "cardano-canonical-ledger";
          compiler-nix-name = lib.mkDefault defaultCompiler;

          modules =
            [{ packages.scls-format.components.library.doCoverage = true; }];

          # Tools to include in the development shell
          shell.tools = {
            cabal = "3.16.0.0";
            haskell-language-server = "2.11.0.0";
            hlint = "3.10";
            fourmolu = fourmoluVersion;
            weeder = "2.10.0";
            cabal-gild = cabalGildVersion;
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

        flake = cardanoCanonicalLedger.flake
          (lib.optionalAttrs (system == "x86_64-linux") {
            # on linux, build/test other supported compilers
            variants = lib.genAttrs [ "ghc984" "ghc9102" "ghc9121" ]
              (compiler-nix-name: { inherit compiler-nix-name; });
          });
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            fourmolu.enable = true;
            nixfmt-classic.enable = true;
            cabal-gild.enable = true;
          };
          tools = {
            fourmolu = cardanoCanonicalLedger.tool "fourmolu" fourmoluVersion;
            cabal-gild =
              cardanoCanonicalLedger.tool "cabal-gild" cabalGildVersion;
          };
        };
        treefmtEval = treefmt-nix.lib.evalModule pkgs ./nix/treefmt.nix;
      in lib.recursiveUpdate flake rec {
        project = cardanoCanonicalLedger;
        legacyPackages = rec { inherit cardanoCanonicalLedger pkgs; };

        checks = {
          formatting = treefmtEval.${pkgs.system}.config.build.check self;
        };

        devShells = let
          mkDevShells = p:
            p.shell.overrideAttrs
            (old: { shellHook = old.shellHook + pre-commit-check.shellHook; });
        in mkDevShells cardanoCanonicalLedger // lib.mapAttrs
        (compiler-nix-name: _:
          let
            p = cardanoCanonicalLedger.appendModule {
              inherit compiler-nix-name;
            };
          in p.shell // (mkDevShells p)) pkgs.haskell-nix.compiler;
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
