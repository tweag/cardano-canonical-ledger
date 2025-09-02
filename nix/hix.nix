{ pkgs, ... }: {
  name = "cardano-canonical-ledger";
  compiler-nix-name = "ghc910"; # Version of GHC to use

  # Cross compilation support:
  # crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
  #   p.mingwW64
  #   p.ghcjs
  # ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
  #   p.musl64
  # ]);

  # Tools to include in the development shell
  shell.tools = {
    cabal = "latest";
    haskell-language-server = "latest";
    hlint = "latest";
    fourmolu = "latest";
    stylish-haskell = "latest";
  };

  # Non-Haskell shell tools go here
  shell.buildInputs = with pkgs; [ nixfmt-classic ];
}
