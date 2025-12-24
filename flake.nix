# SPDX-FileCopyrightText: Chris Montgomery <chmont@protonmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  description = "Example Emacs package";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    devshelves.url = "github:astratagem/devshelves";
    nix-treesitter.url = "github:ratson/nix-treesitter";
    nix-treesitter.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      ...
    }:
    let
      systems = import inputs.systems;

    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      inherit systems;

      imports = [
        inputs.devshelves.flakeModules.default
        inputs.git-hooks.flakeModule
        inputs.treefmt-nix.flakeModule
        ./.config/devshells
        ./.config/git-hooks.nix
        ./.config/treefmt.nix
      ];

      perSystem =
        { system, pkgs, ... }:
        {
          _module.args = {
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ ];
            };
          };

          formatter = pkgs.nixfmt-rfc-style;
        };
    };
}
