<!--
SPDX-FileCopyrightText: (C) 2025 chris montgomery <chmont@protonmail.com>

SPDX-License-Identifier: GPL-3.0-or-later
-->

# corn-ts-mode

A tree-sitter based major mode for editing [Corn](https://cornlang.dev/)
configuration files in Emacs.

## About Corn

Corn is a simple and pain-free configuration language inspired by JSON and Nix.
It provides a minimal, unambiguous syntax that's easy to read and write. Corn
files use the `.corn` extension.

Example Corn file:

```corn
let {
  $colors = {
    primary = "#ff0000"
    secondary = "#00ff00"
  }
} in {
  window = {
    background = $colors.primary
    foreground = $colors.secondary
    width = 800
    height = 600
  }
}
```

## Requirements

- Emacs 29.1+ (built-in tree-sitter support)
- [tree-sitter-corn](https://github.com/corn-config/tree-sitter-corn) grammar

### Installing the Tree-sitter Grammar

You can install the grammar using `treesit-install-language-grammar`:

```elisp
(add-to-list 'treesit-language-source-alist
             '(corn "https://github.com/corn-config/tree-sitter-corn"))

(treesit-install-language-grammar 'corn)
```

## Installation

### From Source

Clone this repository and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/corn-ts-mode")
(require 'corn-ts-mode)
```

### With use-package

```elisp
(use-package corn-ts-mode
  :load-path "/path/to/corn-ts-mode")
```

### With setup.el

Using [setup.el](https://codeberg.org/pkal/setup):

```elisp
(setup (:package corn-ts-mode)
  (setopt corn-ts-mode-indent-offset 4))
```

### With Nix

```nix
{
  inputs.corn-ts-mode.url = "github:astratagem/corn-ts-mode";

  # In your Emacs package configuration:
  # packages = [ inputs.corn-ts-mode.packages.${system}.default ];
}
```

## Features

- **Syntax highlighting** via tree-sitter:
  - Keywords (`let`, `in`)
  - Strings, numbers, booleans, and `null`
  - Comments (`//`)
  - Variables/inputs (`$name`)
  - Object properties and key paths
  - Spread operators (`..`)
  - Brackets and delimiters
- **Smart indentation** with configurable offset (`corn-ts-mode-indent-offset`)
- **Imenu integration** for navigating variables and properties
- **Electric indentation** for `{}` and `[]`
- **Automatic grammar installation** - grammar source is pre-configured

## Development

### Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled
- [direnv](https://direnv.net/) (optional)

### Setup

```bash
# Enter development shell
nix develop

# Or with direnv
direnv allow
```

### Commands

```bash
just test     # Run tests
just lint     # Run linters
just check    # Run all checks
just fmt      # Format code
```

## License

GPL-3.0-or-later. See [LICENSES/GPL-3.0-or-later.txt](LICENSES/GPL-3.0-or-later.txt).

## Resources

- [Corn Language](https://cornlang.dev/) - Official documentation
- [tree-sitter-corn](https://github.com/corn-config/tree-sitter-corn) - Tree-sitter grammar
- [Corn GitHub](https://github.com/corn-config) - Related projects
