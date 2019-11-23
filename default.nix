let
  #haskell-nix = sources."haskell.nix";
  haskell-nix = /home/zimbatm/go/src/github.com/input-output-hk/haskell.nix;

  pkgs = import "${haskell-nix}/nixpkgs" (import haskell-nix);
in
  pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  }
