# SPDX-FileCopyrightText: Chris Montgomery <chmont@protonmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later

###: <https://just.systems/man/en/>

mod reuse '.config/reuse.just'
mod release '.config/release.just'

prj-root := env("PRJ_ROOT")

default:
  @just --choose

# Check the project for issues
[doc("Check the project for issues")]
check:
    just lint
    just test

# Format the project files
[doc("Format the project files")]
fmt:
    treefmt

# Run ERT tests
[doc("Run ERT tests")]
test:
    emacs --batch \
      --eval "(when-let ((path (getenv \"TREESIT_EXTRA_LOAD_PATH\"))) \
                (add-to-list 'treesit-extra-load-path path))" \
      -l ert \
      -l {{prj-root}}/corn-ts-mode.el \
      -l {{prj-root}}/corn-ts-mode-test.el \
      -f ert-run-tests-batch-and-exit

# Run a specific test file
[doc("Run a specific test file")]
test-file FILE:
    emacs --batch \
      --eval "(when-let ((path (getenv \"TREESIT_EXTRA_LOAD_PATH\"))) \
                (add-to-list 'treesit-extra-load-path path))" \
      -l ert \
      -l {{prj-root}}/corn-ts-mode.el \
      -l {{FILE}} \
      -f ert-run-tests-batch-and-exit

# Run all linters
[doc("Run all linters (byte-compile, checkdoc, package-lint)")]
lint:
    just compile
    just checkdoc
    just package-lint

# Byte-compile the package
[doc("Byte-compile the package with warnings as errors")]
compile:
    emacs --batch \
      --eval "(when-let ((path (getenv \"TREESIT_EXTRA_LOAD_PATH\"))) \
                (add-to-list 'treesit-extra-load-path path))" \
      --eval "(setq byte-compile-error-on-warn t)" \
      -f batch-byte-compile {{prj-root}}/corn-ts-mode.el

# Run checkdoc
[doc("Validate documentation strings")]
checkdoc:
    emacs --batch \
      --eval "(require 'checkdoc)" \
      --eval "(setq checkdoc-arguments-in-order-flag t)" \
      --eval "(let ((file \"{{prj-root}}/corn-ts-mode.el\")) \
                (with-current-buffer (find-file-noselect file) \
                  (checkdoc-current-buffer t)))"

# Run package-lint
[doc("Validate package metadata and conventions")]
package-lint:
    emacs --batch \
      --eval "(require 'package)" \
      --eval "(package-initialize)" \
      --eval "(unless (package-installed-p 'package-lint) \
                (package-refresh-contents) \
                (package-install 'package-lint))" \
      --eval "(require 'package-lint)" \
      -f package-lint-batch-and-exit {{prj-root}}/corn-ts-mode.el

# Load the package in an interactive Emacs session
[doc("Load the package in an interactive Emacs session")]
interactive:
    emacs -Q \
      --eval "(when-let ((path (getenv \"TREESIT_EXTRA_LOAD_PATH\"))) \
                (add-to-list 'treesit-extra-load-path path))" \
      -l {{prj-root}}/corn-ts-mode.el

# Rename the package from 'exampel' to a new name
[doc("Rename the package from 'exampel' to a new name")]
rename NEW_NAME:
    #!/usr/bin/env bash
    set -euo pipefail

    OLD_NAME="exampel"
    NEW_NAME="{{NEW_NAME}}"
    PRJ_ROOT="{{prj-root}}"

    if [[ -z "$NEW_NAME" ]]; then
      echo "Error: Please provide a new package name"
      exit 1
    fi

    if [[ "$NEW_NAME" == "$OLD_NAME" ]]; then
      echo "Error: New name is the same as old name"
      exit 1
    fi

    # Validate the new name (must be valid Emacs Lisp symbol)
    if ! [[ "$NEW_NAME" =~ ^[a-z][a-z0-9-]*$ ]]; then
      echo "Error: Package name must start with lowercase letter and contain only lowercase letters, numbers, and hyphens"
      exit 1
    fi

    echo "Renaming package from '$OLD_NAME' to '$NEW_NAME'..."

    # Rename files
    if [[ -f "$PRJ_ROOT/lisp/$OLD_NAME.el" ]]; then
      mv "$PRJ_ROOT/lisp/$OLD_NAME.el" "$PRJ_ROOT/lisp/$NEW_NAME.el"
      echo "  Renamed lisp/$OLD_NAME.el -> lisp/$NEW_NAME.el"
    fi

    if [[ -f "$PRJ_ROOT/test/$OLD_NAME-test.el" ]]; then
      mv "$PRJ_ROOT/test/$OLD_NAME-test.el" "$PRJ_ROOT/test/$NEW_NAME-test.el"
      echo "  Renamed test/$OLD_NAME-test.el -> test/$NEW_NAME-test.el"
    fi

    # Update file contents
    echo "  Updating symbol prefixes and references..."

    # Update the main package file
    if [[ -f "$PRJ_ROOT/lisp/$NEW_NAME.el" ]]; then
      sed -i "s/$OLD_NAME/$NEW_NAME/g" "$PRJ_ROOT/lisp/$NEW_NAME.el"
    fi

    # Update the test file
    if [[ -f "$PRJ_ROOT/test/$NEW_NAME-test.el" ]]; then
      sed -i "s/$OLD_NAME/$NEW_NAME/g" "$PRJ_ROOT/test/$NEW_NAME-test.el"
    fi

    # Update flake.nix
    if [[ -f "$PRJ_ROOT/flake.nix" ]]; then
      sed -i "s/$OLD_NAME/$NEW_NAME/g" "$PRJ_ROOT/flake.nix"
    fi

    # Update Justfile
    if [[ -f "$PRJ_ROOT/Justfile" ]]; then
      sed -i "s/$OLD_NAME/$NEW_NAME/g" "$PRJ_ROOT/Justfile"
    fi

    # Update CLAUDE.md
    if [[ -f "$PRJ_ROOT/CLAUDE.md" ]]; then
      sed -i "s/$OLD_NAME/$NEW_NAME/g" "$PRJ_ROOT/CLAUDE.md"
    fi

    # Update .dir-locals.el if it exists
    if [[ -f "$PRJ_ROOT/.dir-locals.el" ]]; then
      sed -i "s/$OLD_NAME/$NEW_NAME/g" "$PRJ_ROOT/.dir-locals.el"
    fi

    echo "Done. Package renamed to '$NEW_NAME'."
    echo ""
    echo "Please review the changes and update:"
    echo "  - Author/maintainer information in lisp/$NEW_NAME.el"
    echo "  - URL and homepage references"
    echo "  - Any other project-specific metadata"
