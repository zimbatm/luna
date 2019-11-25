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
      identifier = { name = "luna-passes"; version = "0.1"; };
      license = "NONE";
      copyright = "";
      maintainer = "Wojciech Danilo  <wojciech.danilo@luna-lang.org>,\nMarcin Kostrzewa <marcin.kostrzewa@luna-lang.org>,\nAra Adkins       <ara.adkins@luna-lang.org>";
      author = "Luna Team <contact@luna-lang.org>";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."container" or (buildDepError "container"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."layered-state" or (buildDepError "layered-state"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."luna-core" or (buildDepError "luna-core"))
          (hsPkgs."luna-cpp-containers" or (buildDepError "luna-cpp-containers"))
          (hsPkgs."luna-data-construction" or (buildDepError "luna-data-construction"))
          (hsPkgs."luna-data-storable" or (buildDepError "luna-data-storable"))
          (hsPkgs."luna-debug" or (buildDepError "luna-debug"))
          (hsPkgs."luna-exception" or (buildDepError "luna-exception"))
          (hsPkgs."luna-foreign-utils" or (buildDepError "luna-foreign-utils"))
          (hsPkgs."luna-future" or (buildDepError "luna-future"))
          (hsPkgs."luna-generic-traversable" or (buildDepError "luna-generic-traversable"))
          (hsPkgs."luna-runtime" or (buildDepError "luna-runtime"))
          (hsPkgs."luna-syntax-text-builder" or (buildDepError "luna-syntax-text-builder"))
          (hsPkgs."luna-syntax-text-lexer" or (buildDepError "luna-syntax-text-lexer"))
          (hsPkgs."luna-syntax-text-model" or (buildDepError "luna-syntax-text-model"))
          (hsPkgs."luna-syntax-text-parser" or (buildDepError "luna-syntax-text-parser"))
          (hsPkgs."luna-syntax-text-prettyprint" or (buildDepError "luna-syntax-text-prettyprint"))
          (hsPkgs."luna-text-processing" or (buildDepError "luna-text-processing"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."path" or (buildDepError "path"))
          (hsPkgs."prologue" or (buildDepError "prologue"))
          (hsPkgs."storable-tuple" or (buildDepError "storable-tuple"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."uuid-types" or (buildDepError "uuid-types"))
          ];
        buildable = true;
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././passes; }) // {
    cabal-generator = "hpack";
    }