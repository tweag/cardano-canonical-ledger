{ pkgs, ... }: {
  # Used to find the project root
  projectRootFile = "flake.nix";

  programs.nixfmt-classic.enable = true;

  programs.fourmolu.enable = true;
  programs.fourmolu.package =
    pkgs.haskell-nix.tool "ghc910" "fourmolu" "latest";

  programs.cabal-gild.enable = true;
  programs.cabal-gild.package =
    pkgs.haskell-nix.tool "ghc910" "cabal-gild" "latest";
}
