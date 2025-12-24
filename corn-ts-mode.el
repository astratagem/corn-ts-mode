;;; corn-ts-mode.el --- Corn configuration language support -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2025 Chris Montgomery <chmont@protonmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Maintainer: Chris Montgomery <chmont@protonmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience
;; URL: https://github.com/astratagem/corn-ts-mode

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a major mode for editing Corn configuration files,
;; powered by the built-in tree-sitter support in Emacs 29+.
;;
;; Corn is a simple configuration language with support for variables, nested
;; objects, arrays, and string interpolation.  For more information, see
;; <https://github.com/corn-config/corn>.

;;;; Requirements:

;; - Emacs 29.1 or later (for `treesit')
;; - The tree-sitter grammar for Corn, usually packaged as "tree-sitter-corn"

;;;; Installation:

;; To install the grammar automatically with Emacs, add the following to your configuration:
;;
;;     (add-to-list 'treesit-language-source-alist
;;                  '(corn "https://github.com/corn-config/tree-sitter-corn"))
;;
;; Then run `M-x treesit-install-language-grammar RET corn RET'.

;;;; Usage:

;; `corn-ts-mode' associates automatically with "*.corn" files.  You can also
;; invoke `corn-ts-mode' manually.


;;; Code:

;;;; Requirements

(require 'treesit)

(eval-when-compile
  (require 'cl-lib)
  (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-text "treesit.c")

(cl-pushnew '(corn "https://github.com/corn-config/tree-sitter-corn"
               :commit "464654742cbfd3a3de560aba120998f1d5dfa844")
  treesit-language-source-alist)

;;;; Customization

(defgroup corn nil
  "Example package customization group."
  :group 'convenience
  :prefix "corn-ts-mode-"
  :link '(url-link :tag "Homepage" "https://github.com/astratagem/corn-ts-mode")
  :link '(emacs-commentary-link :tag "Commentary" "corn-ts-mode"))

(defcustom corn-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `corn-ts-mode'."
  :type 'integer
  :safe #'natnump
  :group 'corn)

;;;; Variables

(defvar corn-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?. "." table)
    table)
  "Syntax table for `corn-ts-mode'.")

;;;; Keymap

(defvar-keymap corn-ts-mode-map
  :doc "Keymap for `corn-ts-mode-mode'.")

;;;; Commands

;;;; Private Functions

(defun corn-ts-mode--font-lock-settings ()
  "Return the tree-sitter font-lock settings for Corn."
  (treesit-font-lock-rules
    :language 'corn
    :feature 'comment
    '((comment) @font-lock-comment-face)

    :language 'corn
    :feature 'keyword
    '(["let" "in"] @font-lock-keyword-face)

    :language 'corn
    :feature 'constant
    '( (boolean) @font-lock-constant-face
       (null) @font-lock-constant-face)

    :language 'corn
    :feature 'string
    '((string) @font-lock-string-face)

    :language 'corn
    :feature 'number
    '( (integer) @font-lock-number-face
       (float) @font-lock-number-face)

    :language 'corn
    :feature 'variable
    '((input) @font-lock-variable-use-face)

    :language 'corn
    :feature 'property
    '( (pair (path) @font-lock-property-name-face)
       (assignment (input) @font-lock-variable-name-face))

    :language 'corn
    :feature 'operator
    '((spread ".." @font-lock-operator-face))

    :language 'corn
    :feature 'bracket
    '(["[" "]" "{" "}"] @font-lock-bracket-face)

    :language 'corn
    :feature 'delimiter
    '(["=" "."] @font-lock-delimiter-face)))

(defun corn-ts-mode--indent-rules ()
  "Return the tree-sitter indentation rules for Corn."
  (let ((offset corn-ts-mode-indent-offset))
    `((corn
        ((parent-is "source_file") column-0 0)
        ((node-is "}") parent-bol 0)
        ((node-is "]") parent-bol 0)
        ((parent-is "object") parent-bol ,offset)
        ((parent-is "array") parent-bol ,offset)
        ((parent-is "assign_block") parent-bol ,offset)
        (no-node parent-bol 0)))))

(defun corn-ts-mode--defun-name (node)
  "Return the defun name for NODE, or nil if not applicable."
  (pcase (treesit-node-type node)
    ("assignment"
      (when-let* ((input (treesit-node-child-by-field-name node "input")))
        (treesit-node-text input t)))
    ("pair"
      (when-let* ((path (treesit-node-child-by-field-name node "path")))
        (treesit-node-text path t)))))

;;;; Major Mode

;;;###autoload
(define-derived-mode corn-ts-mode prog-mode "Corn"
  "Major mode for editing Corn configuration files, powered by `treesit'.
Corn is a configuration language with support for variables, objects,
arrays, and string interpolation.

\\{corn-ts-mode-map}"
  :group 'corn
  :syntax-table corn-ts-mode-syntax-table

  (unless (treesit-ready-p 'corn)
    (error "Tree-Sitter grammar for Corn is not available; see `corn-ts-mode'
commentary for installation instructions."))

  (treesit-parser-create 'corn)

  ;; Comments.
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx "//" (* (syntax whitespace))))

  ;; Electricity.
  (setq-local electric-indent-chars (append "{}[]" electric-indent-chars))

  ;; Font lock.
  (setq-local treesit-font-lock-settings (corn-ts-mode--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
    '( (comment)
       (keyword string constant)
       (number property variable)
       (bracket delimiter operator)))

  ;; Indentation.
  (setq-local treesit-simple-indent-rules (corn-ts-mode--indent-rules))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp (rx (or "assignment" "pair")))
  (setq-local treesit-defun-name-function #'corn-ts-mode--defun-name)

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
    '( ("Variable" "\\`assignment\\'" nil nil)
       ("Property" "\\`pair\\'" nil nil)))

  (treesit-major-mode-setup))

;;;###autoload
(cl-pushnew '("\\.corn\\'" . corn-ts-mode) auto-mode-alist)

;;;; Footer

(provide 'corn-ts-mode)

;;; corn-ts-mode.el ends here
