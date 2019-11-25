let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  ({
    flags = {};
    package = {
      specVersion = "0";
      identifier = { name = "luna-core"; version = "0.0.6"; };
      license = "Apache-2.0";
      copyright = "Copyright (C) 2018 Luna Team";
      maintainer = "Wojciech Danilo  <wojciech.danilo@luna-lang.org>,\nMarcin Kostrzewa <marcin.kostrzewa@luna-lang.org>,\nAra Adkins       <ara.adkins@luna-lang.org>";
      author = "Luna Team";
      homepage = "https://github.com/luna/luna";
      url = "";
      synopsis = "Luna Core";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."container" or (buildDepError "container"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."convert" or (buildDepError "convert"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."functor-utils" or (buildDepError "functor-utils"))
          (hsPkgs."ghc" or (buildDepError "ghc"))
          (hsPkgs."layered-state" or (buildDepError "layered-state"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."lens-utils" or (buildDepError "lens-utils"))
          (hsPkgs."luna-autovector" or (buildDepError "luna-autovector"))
          (hsPkgs."luna-cpp-containers" or (buildDepError "luna-cpp-containers"))
          (hsPkgs."luna-data-construction" or (buildDepError "luna-data-construction"))
          (hsPkgs."luna-data-property" or (buildDepError "luna-data-property"))
          (hsPkgs."luna-data-storable" or (buildDepError "luna-data-storable"))
          (hsPkgs."luna-data-tag" or (buildDepError "luna-data-tag"))
          (hsPkgs."luna-data-typemap" or (buildDepError "luna-data-typemap"))
          (hsPkgs."luna-exception" or (buildDepError "luna-exception"))
          (hsPkgs."luna-foreign-utils" or (buildDepError "luna-foreign-utils"))
          (hsPkgs."luna-generic-traversable" or (buildDepError "luna-generic-traversable"))
          (hsPkgs."luna-memory-manager" or (buildDepError "luna-memory-manager"))
          (hsPkgs."luna-memory-pool" or (buildDepError "luna-memory-pool"))
          (hsPkgs."luna-nested-containers" or (buildDepError "luna-nested-containers"))
          (hsPkgs."luna-syntax-definition" or (buildDepError "luna-syntax-definition"))
          (hsPkgs."luna-th-builder" or (buildDepError "luna-th-builder"))
          (hsPkgs."luna-tuple-utils" or (buildDepError "luna-tuple-utils"))
          (hsPkgs."luna-type-cache" or (buildDepError "luna-type-cache"))
          (hsPkgs."monad-branch" or (buildDepError "monad-branch"))
          (hsPkgs."monoid" or (buildDepError "monoid"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."primitive" or (buildDepError "primitive"))
          (hsPkgs."prologue" or (buildDepError "prologue"))
          (hsPkgs."random" or (buildDepError "random"))
          (hsPkgs."split" or (buildDepError "split"))
          (hsPkgs."storable-tuple" or (buildDepError "storable-tuple"))
          (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."typelevel" or (buildDepError "typelevel"))
          (hsPkgs."vector" or (buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."container" or (buildDepError "container"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-expectations-lifted" or (buildDepError "hspec-expectations-lifted"))
            (hsPkgs."luna-core" or (buildDepError "luna-core"))
            (hsPkgs."luna-cpp-containers" or (buildDepError "luna-cpp-containers"))
            (hsPkgs."luna-data-storable" or (buildDepError "luna-data-storable"))
            (hsPkgs."luna-exception" or (buildDepError "luna-exception"))
            (hsPkgs."luna-foreign-utils" or (buildDepError "luna-foreign-utils"))
            (hsPkgs."luna-generic-traversable" or (buildDepError "luna-generic-traversable"))
            (hsPkgs."prologue" or (buildDepError "prologue"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."typelevel" or (buildDepError "typelevel"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "array-benchmark" = {
          depends = [
            (hsPkgs."ansi-terminal" or (buildDepError "ansi-terminal"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."convert" or (buildDepError "convert"))
            (hsPkgs."criterion" or (buildDepError "criterion"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."ghc" or (buildDepError "ghc"))
            (hsPkgs."layered-state" or (buildDepError "layered-state"))
            (hsPkgs."luna-autovector" or (buildDepError "luna-autovector"))
            (hsPkgs."luna-core" or (buildDepError "luna-core"))
            (hsPkgs."luna-cpp-containers" or (buildDepError "luna-cpp-containers"))
            (hsPkgs."luna-data-storable" or (buildDepError "luna-data-storable"))
            (hsPkgs."luna-data-typemap" or (buildDepError "luna-data-typemap"))
            (hsPkgs."luna-exception" or (buildDepError "luna-exception"))
            (hsPkgs."luna-foreign-utils" or (buildDepError "luna-foreign-utils"))
            (hsPkgs."luna-generic-traversable" or (buildDepError "luna-generic-traversable"))
            (hsPkgs."luna-generic-traversable2" or (buildDepError "luna-generic-traversable2"))
            (hsPkgs."luna-memory-manager" or (buildDepError "luna-memory-manager"))
            (hsPkgs."luna-memory-pool" or (buildDepError "luna-memory-pool"))
            (hsPkgs."luna-tuple-utils" or (buildDepError "luna-tuple-utils"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."primitive" or (buildDepError "primitive"))
            (hsPkgs."prologue" or (buildDepError "prologue"))
            (hsPkgs."structs" or (buildDepError "structs"))
            (hsPkgs."unboxed-ref" or (buildDepError "unboxed-ref"))
            (hsPkgs."vector" or (buildDepError "vector"))
            ];
          libs = [ (pkgs."stdc++" or (sysDepError "stdc++")) ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././core; }) // {
    cabal-generator = "hpack";
    }