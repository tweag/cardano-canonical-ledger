project:
{ ... }: {
  # Used to find the project root
  projectRootFile = "flake.nix";

  programs.nixfmt-classic.enable = true;

  programs.fourmolu.enable = true;
  programs.fourmolu.package =
    project.cardanoCanonicalLedger.tool "fourmolu" project.fourmoluVersion;

  programs.cabal-gild.enable = true;
  programs.cabal-gild.package =
    project.cardanoCanonicalLedger.tool "cabal-gild" project.cabalGildVersion;
}
