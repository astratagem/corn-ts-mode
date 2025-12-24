# CLAUDE.md - Project Guidelines

This is an Emacs Lisp package that provides `corn-ts-mode`, a tree-sitter based
major mode for editing [Corn](https://cornlang.dev/) configuration files.

## About Corn

Corn is a simple and pain-free configuration language inspired by JSON and Nix.
It provides:

- A minimal feature set that can be learned in minutes
- Simple, unambiguous syntax borrowing from established languages
- Human-readable and writable configuration files
- Key-chaining for creating deeply nested objects
- Inputs with merging to reduce repetition

Corn files use the `.corn` extension.

## Project Structure

```text
corn-ts-mode/
├── corn-ts-mode.el         # Main package file (tree-sitter major mode)
├── corn-ts-mode-test.el    # ERT tests
├── LICENSES/
│   └── GPL-3.0-or-later.txt
├── flake.nix               # Nix flake with package outputs
├── justfile                # Task runner recipes
├── .dir-locals.el          # Emacs project settings
└── CLAUDE.md               # Project guidelines (this file)
```

## Development Setup

### Prerequisites

- Nix with flakes enabled
- direnv (optional, for automatic environment loading)
- Emacs 29.1+ (required for built-in tree-sitter support)

### Entering the Development Shell

```bash
# With direnv (automatic)
direnv allow

# Manual
nix develop
```

The development shell provides:

- Emacs with package dependencies
- Testing tools (package-lint, checkdoc)
- Formatting tools (treefmt, lisp-format)
- Just task runner
- REUSE licensing tools

### Tree-sitter Grammar

This mode requires the tree-sitter-corn grammar. The grammar is available at:
https://github.com/corn-config/tree-sitter-corn

Note: As of late 2024, tree-sitter-corn may not be in nixpkgs. You may need to
build it from source or use an overlay.

## Common Tasks

All tasks are run via Just:

```bash
just              # Interactive menu
just test         # Run ERT tests
just lint         # Run all linters (package-lint, checkdoc, byte-compile)
just compile      # Byte-compile the package
just fmt          # Format all files
just check        # Run all checks
```

### Testing

Tests use ERT (Emacs Regression Testing), the built-in testing framework:

```bash
just test                                  # Run all tests
just test-file corn-ts-mode-test.el        # Run specific test file
```

### Linting

For MELPA compliance, the package must pass:

- **package-lint**: Validates package metadata and naming conventions
- **checkdoc**: Validates documentation strings
- **byte-compile**: Must compile without warnings

```bash
just lint           # Run all linters
just package-lint   # Package metadata only
just checkdoc       # Documentation only
```

## Code Style

### Emacs Lisp Conventions

- Use `lexical-binding: t` in all Elisp files
- Prefix all symbols with `corn-ts-mode-` (the package name)
- Use `corn-ts-mode--` prefix for private/internal functions
- Follow [GNU Emacs Lisp conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html)

### File Headers

All Elisp files must include standard headers:

```elisp
;;; corn-ts-mode.el --- Corn configuration language support -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2025 Chris Montgomery <chmont@protonmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Maintainer: Chris Montgomery <chmont@protonmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience
;; URL: https://github.com/astratagem/corn-ts-mode

;;; Commentary:

;; Tree-sitter based major mode for editing Corn configuration files.

;;; Code:

(provide 'corn-ts-mode)
;;; corn-ts-mode.el ends here
```

### Documentation

- All public functions must have docstrings
- Use `checkdoc` to validate documentation
- First line of docstring should be a complete sentence

## MELPA Submission

Before submitting to MELPA:

1. Ensure all lints pass: `just lint`
2. Ensure tests pass: `just test`
3. Verify package installs correctly from local source
4. Create a recipe file (see MELPA contributing guide)

Example MELPA recipe:

```elisp
(corn-ts-mode :fetcher github
              :repo "astratagem/corn-ts-mode")
```

## Nix Integration

### Building the Package

```bash
nix build           # Build the package
nix build .#checks  # Run all checks
```

### Flake Outputs

- `packages.<system>.corn-ts-mode` - The Emacs package
- `packages.<system>.default` - Alias to corn-ts-mode
- `checks.<system>.test` - ERT test suite
- `checks.<system>.lint` - Linting checks
- `devShells.<system>.default` - Development environment

## License

This project is licensed under GPL-3.0-or-later. All files must include
SPDX headers for REUSE compliance.

```bash
just reuse::lint              # Check REUSE compliance
just reuse::gpl *.el          # Add GPL headers to files
```

## Resources

- [Corn Language](https://cornlang.dev/) - Official Corn documentation
- [tree-sitter-corn](https://github.com/corn-config/tree-sitter-corn) - Tree-sitter grammar
- [Corn GitHub Organization](https://github.com/corn-config) - Related projects
