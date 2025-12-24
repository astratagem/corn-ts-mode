# SPDX-FileCopyrightText: Chris Montgomery <chmont@protonmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later
{ inputs, lib, ... }:
{
  perSystem =
    {
      config,
      inputs',
      pkgs,
      ...
    }:
    let
      # Tree-sitter grammar for Corn from nix-treesitter
      tree-sitter-corn = inputs'.nix-treesitter.packages.tree-sitter-corn;

      # Directory containing tree-sitter grammars for Emacs
      treesitGrammars = pkgs.runCommand "treesit-grammars" { } ''
        mkdir -p $out
        ln -s ${tree-sitter-corn}/parser $out/libtree-sitter-corn.so
      '';

      # Emacs with development packages
      emacsWithPackages = pkgs.emacsPackages.emacsWithPackages (epkgs: [
        epkgs.package-lint
      ]);

      nativeBuildInputs = [ ];

      buildInputs = [ ];

      commonPkgs =
        buildInputs
        ++ nativeBuildInputs
        ++ [
          inputs'.nixpkgs-trunk.legacyPackages.just
          pkgs.reuse
        ];

      # Emacs development tools
      emacsPkgs = [
        emacsWithPackages
      ];

      checksPkgs = config.pre-commit.settings.enabledPackages ++ [
        pkgs.biome
      ];

      formatterPkgs = (lib.attrValues config.treefmt.build.programs) ++ [
        config.formatter
        config.treefmt.build.wrapper
      ];

      ciPkgs = commonPkgs ++ checksPkgs ++ emacsPkgs;
      devPkgs = commonPkgs ++ checksPkgs ++ formatterPkgs ++ emacsPkgs ++ [ ];

      shellHook = ''
        : "''${PRJ_BIN_HOME:=''${PRJ_PATH:-''${PRJ_ROOT}/.bin}}"

        export PRJ_BIN_HOME

        ${config.pre-commit.installationScript}
      '';
    in
    {
      shells.default = {
        inherit shellHook;
        name = "corn-ts-mode-dev";
        packages = devPkgs;
        env = {
          # Make tree-sitter-corn grammar available to Emacs
          TREESIT_EXTRA_LOAD_PATH = treesitGrammars;
        };
      };

      shells.ci = {
        packages = ciPkgs;
        env = {
          TREESIT_EXTRA_LOAD_PATH = treesitGrammars;
        };
      };
    };
}
