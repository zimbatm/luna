let
  haskell-nix = builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz;

  pkgs = import "${haskell-nix}/nixpkgs" (import haskell-nix);

  pkgSet = pkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
