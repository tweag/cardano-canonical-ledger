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
  };

  # Non-Haskell shell tools go here
  shell.buildInputs = with pkgs; [ nixfmt-classic ];
}
