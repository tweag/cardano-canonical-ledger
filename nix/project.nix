pkgs:
let
  defaultCompiler = "ghc910";
  fourmoluVersion = "0.19.0.0";
  cabalGildVersion = "1.6.0.2";
in {
  inherit fourmoluVersion cabalGildVersion;

  cardanoCanonicalLedger = pkgs.haskell-nix.cabalProject' {
    src = ./..;

    name = "cardano-canonical-ledger";
    compiler-nix-name = pkgs.lib.mkDefault defaultCompiler;

    modules = [{ packages.scls-format.components.library.doCoverage = true; }];

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
}
