{ pkgs, ... }: {
  name = "cardano-canonical-ledger";
  compiler-nix-name = "ghc910"; # Version of GHC to use

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
    hls-wrapper = pkgs.writeShellScriptBin "haskell-language-server-wrapper" ''
      exec haskell-language-server "$@"
    '';
  in with pkgs; [ nixfmt-classic hls-wrapper ];
}
