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
      identifier = { name = "luna-shell"; version = "1.2"; };
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
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bimap" or (buildDepError "bimap"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."githash" or (buildDepError "githash"))
          (hsPkgs."layered-state" or (buildDepError "layered-state"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."luna-core" or (buildDepError "luna-core"))
          (hsPkgs."luna-datafile" or (buildDepError "luna-datafile"))
          (hsPkgs."luna-exception" or (buildDepError "luna-exception"))
          (hsPkgs."luna-package" or (buildDepError "luna-package"))
          (hsPkgs."luna-passes" or (buildDepError "luna-passes"))
          (hsPkgs."luna-path" or (buildDepError "luna-path"))
          (hsPkgs."luna-runtime" or (buildDepError "luna-runtime"))
          (hsPkgs."luna-stdlib" or (buildDepError "luna-stdlib"))
          (hsPkgs."megaparsec" or (buildDepError "megaparsec"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."path" or (buildDepError "path"))
          (hsPkgs."path-io" or (buildDepError "path-io"))
          (hsPkgs."prologue" or (buildDepError "prologue"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ];
        buildable = true;
        };
      exes = {
        "luna" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."luna-shell" or (buildDepError "luna-shell"))
            (hsPkgs."prologue" or (buildDepError "prologue"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "interpreter-benchmark" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."luna-datafile" or (buildDepError "luna-datafile"))
            (hsPkgs."luna-exception" or (buildDepError "luna-exception"))
            (hsPkgs."luna-package" or (buildDepError "luna-package"))
            (hsPkgs."luna-shell" or (buildDepError "luna-shell"))
            (hsPkgs."path" or (buildDepError "path"))
            (hsPkgs."prologue" or (buildDepError "prologue"))
            (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././shell; }) // {
    cabal-generator = "hpack";
    }