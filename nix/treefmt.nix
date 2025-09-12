{ pkgs, ... }: {
  # Used to find the project root
  projectRootFile = "flake.nix";
  # Enable the fourmolu formatter
  programs.fourmolu.enable = true;
  programs.nixfmt-classic.enable = true;
  programs.cabal-gild.enable = true;
}
