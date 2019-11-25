{
  extras = hackage:
    {
      packages = {
        luna-core = ./luna-core.nix;
        luna-debug = ./luna-debug.nix;
        luna-passes = ./luna-passes.nix;
        luna-package = ./luna-package.nix;
        luna-path = ./luna-path.nix;
        luna-runtime = ./luna-runtime.nix;
        luna-stdlib = ./luna-stdlib.nix;
        luna-shell = ./luna-shell.nix;
        luna-syntax-text-lexer = ./luna-syntax-text-lexer.nix;
        luna-syntax-text-model = ./luna-syntax-text-model.nix;
        luna-syntax-text-parser = ./luna-syntax-text-parser.nix;
        luna-syntax-text-builder = ./luna-syntax-text-builder.nix;
        luna-syntax-text-prettyprint = ./luna-syntax-text-prettyprint.nix;
        luna-autovector = ./luna-autovector.nix;
        luna-ci = ./luna-ci.nix;
        luna-code-builder = ./luna-code-builder.nix;
        container = ./container.nix;
        convert = ./convert.nix;
        luna-cpp-containers = ./luna-cpp-containers.nix;
        luna-data-construction = ./luna-data-construction.nix;
        data-poset = ./data-poset.nix;
        luna-data-property = ./luna-data-property.nix;
        luna-data-storable = ./luna-data-storable.nix;
        luna-data-tag = ./luna-data-tag.nix;
        luna-data-typemap = ./luna-data-typemap.nix;
        luna-datafile = ./luna-datafile.nix;
        luna-exception = ./luna-exception.nix;
        luna-foreign-utils = ./luna-foreign-utils.nix;
        functor-utils = ./functor-utils.nix;
        luna-future = ./luna-future.nix;
        luna-generic-traversable = ./luna-generic-traversable.nix;
        luna-generic-traversable2 = ./luna-generic-traversable2.nix;
        hspec-jenkins = ./hspec-jenkins.nix;
        impossible = ./impossible.nix;
        layered-state = ./layered-state.nix;
        layouting = ./layouting.nix;
        lens-utils = ./lens-utils.nix;
        luna-memory-manager = ./luna-memory-manager.nix;
        luna-memory-pool = ./luna-memory-pool.nix;
        monad-branch = ./monad-branch.nix;
        monoid = ./monoid.nix;
        luna-nested-containers = ./luna-nested-containers.nix;
        luna-parser-utils = ./luna-parser-utils.nix;
        prologue = ./prologue.nix;
        luna-syntax-definition = ./luna-syntax-definition.nix;
        terminal-text = ./terminal-text.nix;
        luna-text-processing = ./luna-text-processing.nix;
        luna-th-builder = ./luna-th-builder.nix;
        luna-tuple-utils = ./luna-tuple-utils.nix;
        luna-type-cache = ./luna-type-cache.nix;
        typelevel = ./typelevel.nix;
        vector-text = ./vector-text.nix;
        luna-yaml-utils = ./luna-yaml-utils.nix;
        };
      };
  resolver = "lts-12.26";
  modules = [
    ({ lib, ... }:
      { packages = {}; })
    {
      packages = {
        "$everything" = {
          package = { ghcOptions = "-fconstraint-solver-iterations=100"; };
          };
        "$locals" = {
          package = {
            ghcOptions = "-O1 -Wall -Wno-name-shadowing -fexcess-precision -fexpose-all-unfoldings -flate-dmd-anal -fmax-worker-args=1000 -fsimpl-tick-factor=400 -fspec-constr-keen -fspecialise-aggressively -fstatic-argument-transformation -funbox-strict-fields -threaded -fomit-interface-pragmas";
            };
          };
        };
      }
    ];
  }