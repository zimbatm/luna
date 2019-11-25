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
      identifier = { name = "luna-syntax-text-parser"; version = "0.2"; };
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
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."container" or (buildDepError "container"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."convert" or (buildDepError "convert"))
          (hsPkgs."layered-state" or (buildDepError "layered-state"))
          (hsPkgs."layouting" or (buildDepError "layouting"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."lens-utils" or (buildDepError "lens-utils"))
          (hsPkgs."luna-core" or (buildDepError "luna-core"))
          (hsPkgs."luna-cpp-containers" or (buildDepError "luna-cpp-containers"))
          (hsPkgs."luna-data-storable" or (buildDepError "luna-data-storable"))
          (hsPkgs."luna-foreign-utils" or (buildDepError "luna-foreign-utils"))
          (hsPkgs."luna-generic-traversable" or (buildDepError "luna-generic-traversable"))
          (hsPkgs."luna-nested-containers" or (buildDepError "luna-nested-containers"))
          (hsPkgs."luna-parser-utils" or (buildDepError "luna-parser-utils"))
          (hsPkgs."luna-syntax-definition" or (buildDepError "luna-syntax-definition"))
          (hsPkgs."luna-syntax-text-lexer" or (buildDepError "luna-syntax-text-lexer"))
          (hsPkgs."luna-syntax-text-model" or (buildDepError "luna-syntax-text-model"))
          (hsPkgs."luna-text-processing" or (buildDepError "luna-text-processing"))
          (hsPkgs."megaparsec" or (buildDepError "megaparsec"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."prologue" or (buildDepError "prologue"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."vector-text" or (buildDepError "vector-text"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."container" or (buildDepError "container"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."convert" or (buildDepError "convert"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-core" or (buildDepError "hspec-core"))
            (hsPkgs."hspec-expectations-lifted" or (buildDepError "hspec-expectations-lifted"))
            (hsPkgs."hspec-megaparsec" or (buildDepError "hspec-megaparsec"))
            (hsPkgs."layered-state" or (buildDepError "layered-state"))
            (hsPkgs."layouting" or (buildDepError "layouting"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lens-utils" or (buildDepError "lens-utils"))
            (hsPkgs."luna-core" or (buildDepError "luna-core"))
            (hsPkgs."luna-cpp-containers" or (buildDepError "luna-cpp-containers"))
            (hsPkgs."luna-data-storable" or (buildDepError "luna-data-storable"))
            (hsPkgs."luna-foreign-utils" or (buildDepError "luna-foreign-utils"))
            (hsPkgs."luna-generic-traversable" or (buildDepError "luna-generic-traversable"))
            (hsPkgs."luna-nested-containers" or (buildDepError "luna-nested-containers"))
            (hsPkgs."luna-parser-utils" or (buildDepError "luna-parser-utils"))
            (hsPkgs."luna-syntax-definition" or (buildDepError "luna-syntax-definition"))
            (hsPkgs."luna-syntax-text-lexer" or (buildDepError "luna-syntax-text-lexer"))
            (hsPkgs."luna-syntax-text-model" or (buildDepError "luna-syntax-text-model"))
            (hsPkgs."luna-syntax-text-parser" or (buildDepError "luna-syntax-text-parser"))
            (hsPkgs."luna-text-processing" or (buildDepError "luna-text-processing"))
            (hsPkgs."megaparsec" or (buildDepError "megaparsec"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."open-browser" or (buildDepError "open-browser"))
            (hsPkgs."prologue" or (buildDepError "prologue"))
            (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."vector" or (buildDepError "vector"))
            (hsPkgs."vector-text" or (buildDepError "vector-text"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "array-benchmark" = {
          depends = [
            (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."container" or (buildDepError "container"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."convert" or (buildDepError "convert"))
            (hsPkgs."criterion" or (buildDepError "criterion"))
            (hsPkgs."frisby" or (buildDepError "frisby"))
            (hsPkgs."layered-state" or (buildDepError "layered-state"))
            (hsPkgs."layouting" or (buildDepError "layouting"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lens-utils" or (buildDepError "lens-utils"))
            (hsPkgs."luna-core" or (buildDepError "luna-core"))
            (hsPkgs."luna-cpp-containers" or (buildDepError "luna-cpp-containers"))
            (hsPkgs."luna-data-storable" or (buildDepError "luna-data-storable"))
            (hsPkgs."luna-foreign-utils" or (buildDepError "luna-foreign-utils"))
            (hsPkgs."luna-generic-traversable" or (buildDepError "luna-generic-traversable"))
            (hsPkgs."luna-nested-containers" or (buildDepError "luna-nested-containers"))
            (hsPkgs."luna-parser-utils" or (buildDepError "luna-parser-utils"))
            (hsPkgs."luna-syntax-definition" or (buildDepError "luna-syntax-definition"))
            (hsPkgs."luna-syntax-text-lexer" or (buildDepError "luna-syntax-text-lexer"))
            (hsPkgs."luna-syntax-text-model" or (buildDepError "luna-syntax-text-model"))
            (hsPkgs."luna-text-processing" or (buildDepError "luna-text-processing"))
            (hsPkgs."megaparsec" or (buildDepError "megaparsec"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."prologue" or (buildDepError "prologue"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."vector" or (buildDepError "vector"))
            (hsPkgs."vector-text" or (buildDepError "vector-text"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././syntax/text/parser; }) // {
    cabal-generator = "hpack";
    }