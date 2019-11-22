let
  sources = import ./nix/sources.nix;

  pkgs = import sources.nixpkgs (import sources."haskell.nix");
in
  pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  }
