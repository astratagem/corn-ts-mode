# SPDX-FileCopyrightText: Chris Montgomery <chmont@protonmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  perSystem =
    { pkgs, ... }:
    {
      treefmt = {
        projectRootFile = ".git/config";
        programs = {
          nixfmt.enable = true;
        };
        settings.formatter = {
          biome = {
            command = pkgs.biome;
            options = [
              "check"
              "--write"
              "--no-errors-on-unmatched"
              "--files-ignore-unknown=true"
            ];
            includes = [
              "*.js"
              "*.mjs"
              "*.cjs"
              "*.ts"
              "*.mts"
              "*.cts"
              "*.d.ts"
              "*.d.mts"
              "*.d.cts"
              "*.jsx"
              "*.tsx"
              "*.json"
              "*.jsonc"
              "*.css"
              "*.vue"
              "*.svelte"
              "*.astro"
              "*.graphql"
              "*.gql"
            ];
          };
        };
      };
    };
}
