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
      identifier = { name = "luna-package"; version = "0.2"; };
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
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."filemanip" or (buildDepError "filemanip"))
          (hsPkgs."hspec" or (buildDepError "hspec"))
          (hsPkgs."hspec-megaparsec" or (buildDepError "hspec-megaparsec"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."lens-utils" or (buildDepError "lens-utils"))
          (hsPkgs."luna-core" or (buildDepError "luna-core"))
          (hsPkgs."luna-datafile" or (buildDepError "luna-datafile"))
          (hsPkgs."luna-exception" or (buildDepError "luna-exception"))
          (hsPkgs."luna-parser-utils" or (buildDepError "luna-parser-utils"))
          (hsPkgs."luna-path" or (buildDepError "luna-path"))
          (hsPkgs."luna-syntax-text-lexer" or (buildDepError "luna-syntax-text-lexer"))
          (hsPkgs."megaparsec" or (buildDepError "megaparsec"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."path" or (buildDepError "path"))
          (hsPkgs."path-io" or (buildDepError "path-io"))
          (hsPkgs."prologue" or (buildDepError "prologue"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bimap" or (buildDepError "bimap"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."filemanip" or (buildDepError "filemanip"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-jenkins" or (buildDepError "hspec-jenkins"))
            (hsPkgs."hspec-megaparsec" or (buildDepError "hspec-megaparsec"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lens-utils" or (buildDepError "lens-utils"))
            (hsPkgs."luna-ci" or (buildDepError "luna-ci"))
            (hsPkgs."luna-core" or (buildDepError "luna-core"))
            (hsPkgs."luna-datafile" or (buildDepError "luna-datafile"))
            (hsPkgs."luna-exception" or (buildDepError "luna-exception"))
            (hsPkgs."luna-package" or (buildDepError "luna-package"))
            (hsPkgs."luna-parser-utils" or (buildDepError "luna-parser-utils"))
            (hsPkgs."luna-path" or (buildDepError "luna-path"))
            (hsPkgs."luna-syntax-text-lexer" or (buildDepError "luna-syntax-text-lexer"))
            (hsPkgs."luna-yaml-utils" or (buildDepError "luna-yaml-utils"))
            (hsPkgs."megaparsec" or (buildDepError "megaparsec"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."open-browser" or (buildDepError "open-browser"))
            (hsPkgs."path" or (buildDepError "path"))
            (hsPkgs."path-io" or (buildDepError "path-io"))
            (hsPkgs."prologue" or (buildDepError "prologue"))
            (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
            (hsPkgs."temporary-rc" or (buildDepError "temporary-rc"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././package; }) // {
    cabal-generator = "hpack";
    }