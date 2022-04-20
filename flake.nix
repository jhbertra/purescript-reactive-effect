{
  description = "Push-pull FRP with a model implementation";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    rnix-lsp = {
      url = "github:nix-community/rnix-lsp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils, easy-purescript-nix, rnix-lsp, gitignore }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
          easy-ps = import easy-purescript-nix { inherit pkgs; };
          spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

          inherit (gitignore.lib) gitignoreSource;
          inherit (easy-ps) dhall-simple purs-tidy spago psa purs;
          inherit (pkgs) git writeShellScriptBin nodePackages mkShell nodejs nixpkgs-fmt;
          inherit (nodePackages) bower prettier;
          inherit (builtins) concatStringsSep;

          src = gitignoreSource ./.;


          writeShellScriptBinInRepoRoot = name: script: writeShellScriptBin name ''
            cd `${git}/bin/git rev-parse --show-toplevel`
            ${script}
          '';

          extensionsToRegex = extensions: "\\.(${concatStringsSep "|" extensions})";

          formatter = name: cmd: extensions: ''
            echo formatting with ${name}
            ${git}/bin/git ls-files | grep -E '${extensionsToRegex extensions}' | xargs -d $'\\n' ${cmd}
          '';

          dhall-batch = writeShellScriptBin "dhall" ''
            for f in "$@"; do ${dhall-simple}/bin/dhall format --inplace $f; done
          '';

          purs-tidy-hook = {
            enable = true;
            name = "purs-tidy";
            entry = "${purs-tidy}/bin/purs-tidy format-in-place";
            files = "\\.purs$";
            language = "system";
          };

          dhall-hook = {
            enable = true;
            name = "dhall";
            entry = "${dhall-batch}/bin/dhall";
            files = "\\.dhall$";
            language = "system";
          };

          prettier-hook = {
            enable = true;
            types_or = [ "javascript" "css" "html" ];
          };

          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            inherit src;
            hooks = {
              nixpkgs-fmt = {
                enable = true;
                excludes = [ ".*spago-packages.nix$" ];
              };
              prettier = prettier-hook;
              inherit purs-tidy-hook dhall-hook;
            };
          };

          clean = writeShellScriptBinInRepoRoot "clean" ''
            echo cleaning project...
            rm -rf .spago .spago2nix output .psa-stash
            echo removed .spago
            echo removed .spago2nix
            echo removed .psa-stash
            echo removed output
            echo done.
          '';

          psa-args = "--strict --stash --censor-lib --is-lib=.spago";

          runSpago = cmd: ''
            ${spago}/bin/spago ${cmd} --purs-args "${psa-args} --stash"
          '';

          getGlob = { name, version, ... }: ''".spago/${name}/${version}/src/**/*.purs"'';

          spagoSources =
            builtins.toString
              (builtins.map getGlob (builtins.attrValues spagoPkgs.inputs));

          build = writeShellScriptBin "build" (runSpago "build");
          test = writeShellScriptBin "test" (runSpago "test");
          clean-build = writeShellScriptBin "clean-build" ''
            ${clean}/bin/clean
            ${build}/bin/build
          '';

          format = writeShellScriptBin "format" ''
            set -e
            ${formatter "purs-tidy" "${purs-tidy}/bin/purs-tidy format-in-place" ["purs"]}
            ${formatter "dhall" "${dhall-batch}/bin/dhall" ["dhall"]}
            ${formatter "prettier" "${prettier}/bin/prettier -w" ["js" "ts" "css" "html"]
            }
            echo done.
          '';

          frp =
            pkgs.stdenv.mkDerivation {
              name = "purescript-frp";
              buildInputs = [
                spagoPkgs.installSpagoStyle
              ];
              nativeBuildInputs = [ psa purs nodejs ];
              inherit src;
              unpackPhase = ''
                cp -r $src/* .
                install-spago-style
              '';
              buildPhase = ''
                set -e
                echo building project...
                psa compile ${psa-args} ${spagoSources} "./src/**/*.purs"
                echo done.
              '';
              installPhase = ''
                mkdir $out
                mv output $out/
              '';
              doCheck = true;
              checkPhase = ''
                set -e
                psa compile ${psa-args} ${spagoSources} "./src/**/*.purs" "./test/**/*.purs"
                node -e 'require("./output/Test.Main/index").main()'
              '';
            };

        in
        {
          defaultPackage = frp;
          devShell = mkShell {
            buildInputs = [
              build
              clean
              clean-build
              format
              nixpkgs-fmt
              rnix-lsp.defaultPackage."${system}"
              test
              prettier
              bower
            ] ++ easy-ps.buildInputs;
            inherit (pre-commit-check) shellHook;
          };
        }
      );
}
