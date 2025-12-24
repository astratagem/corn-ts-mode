;;; corn-ts-mode-test.el --- Tests for corn-ts-mode.el -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2025 Chris Montgomery <chmont@protonmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the corn-ts-mode package.
;;
;; Run tests with:
;;   emacs -batch -l ert -l corn-ts-mode.el -l corn-ts-mode-test.el -f ert-run-tests-batch-and-exit
;;
;; Or via Just:
;;   just test
;;
;; Note: Some tests require the tree-sitter-corn grammar to be installed.
;; Tests that require the grammar will be skipped if it's not available.

;;; Code:

(require 'ert)
(require 'corn-ts-mode)

;;;; Test Helpers

(defun corn-ts-mode-test--grammar-available-p ()
  "Return non-nil if the Corn tree-sitter grammar is available."
  (treesit-ready-p 'corn t))

(defmacro corn-ts-mode-test--with-grammar (&rest body)
  "Execute BODY only if the Corn grammar is available, otherwise skip."
  (declare (indent 0))
  `(if (corn-ts-mode-test--grammar-available-p)
       (progn ,@body)
     (ert-skip "tree-sitter-corn grammar not available")))

;;;; Customization Tests

(ert-deftest corn-ts-mode-test-customization-group ()
  "Test that the customization group exists."
  (should (get 'corn 'group-documentation)))

(ert-deftest corn-ts-mode-test-indent-offset-default ()
  "Test that `corn-ts-mode-indent-offset' has correct default."
  (should (boundp 'corn-ts-mode-indent-offset))
  (should (integerp corn-ts-mode-indent-offset))
  (should (= corn-ts-mode-indent-offset 2)))

(ert-deftest corn-ts-mode-test-indent-offset-safe-predicate ()
  "Test that `corn-ts-mode-indent-offset' has a safe local variable predicate."
  (should (safe-local-variable-p 'corn-ts-mode-indent-offset 2))
  (should (safe-local-variable-p 'corn-ts-mode-indent-offset 4))
  (should-not (safe-local-variable-p 'corn-ts-mode-indent-offset -1))
  (should-not (safe-local-variable-p 'corn-ts-mode-indent-offset "string")))

;;;; Syntax Table Tests

(ert-deftest corn-ts-mode-test-syntax-table-exists ()
  "Test that the syntax table is defined."
  (should (boundp 'corn-ts-mode-syntax-table))
  (should (syntax-table-p corn-ts-mode-syntax-table)))

(ert-deftest corn-ts-mode-test-syntax-table-comments ()
  "Test that comment syntax is configured correctly."
  ;; `/' should start comments (syntax class `. 12')
  (with-syntax-table corn-ts-mode-syntax-table
    ;; Check that / has punctuation syntax with comment flags
    (should (eq (char-syntax ?/) ?.))
    ;; Newline should end comments
    (should (eq (char-syntax ?\n) ?>))))

(ert-deftest corn-ts-mode-test-syntax-table-strings ()
  "Test that string delimiters are configured."
  (with-syntax-table corn-ts-mode-syntax-table
    ;; Double quotes should be string delimiters
    (should (eq (char-syntax ?\") ?\"))
    ;; Single quotes should also be string delimiters
    (should (eq (char-syntax ?\') ?\"))))

(ert-deftest corn-ts-mode-test-syntax-table-symbols ()
  "Test that symbol constituents are configured."
  (with-syntax-table corn-ts-mode-syntax-table
    ;; `$' and `_' should be symbol constituents
    (should (eq (char-syntax ?$) ?_))
    (should (eq (char-syntax ?_) ?_))))

;;;; Keymap Tests

(ert-deftest corn-ts-mode-test-keymap-exists ()
  "Test that the keymap is defined."
  (should (boundp 'corn-ts-mode-map))
  (should (keymapp corn-ts-mode-map)))

;;;; Auto-mode-alist Tests

(ert-deftest corn-ts-mode-test-auto-mode-alist ()
  "Test that `.corn' files are associated with `corn-ts-mode'."
  (should (assoc "\\.corn\\'" auto-mode-alist))
  (should (eq (cdr (assoc "\\.corn\\'" auto-mode-alist)) 'corn-ts-mode)))

;;;; Grammar Source Tests

(ert-deftest corn-ts-mode-test-grammar-source-registered ()
  "Test that the Corn grammar source is registered."
  (should (assq 'corn treesit-language-source-alist))
  (let ((source (cdr (assq 'corn treesit-language-source-alist))))
    (should (stringp (car source)))
    (should (string-match-p "tree-sitter-corn" (car source)))))

;;;; Private Function Tests

(ert-deftest corn-ts-mode-test-font-lock-settings ()
  "Test that font-lock settings can be generated."
  (corn-ts-mode-test--with-grammar
    (let ((settings (corn-ts-mode--font-lock-settings)))
      (should settings)
      (should (listp settings)))))

(ert-deftest corn-ts-mode-test-indent-rules ()
  "Test that indent rules are generated correctly."
  (let ((rules (corn-ts-mode--indent-rules)))
    (should rules)
    (should (listp rules))
    ;; First element should be the language symbol
    (should (eq (caar rules) 'corn))))

(ert-deftest corn-ts-mode-test-indent-rules-uses-offset ()
  "Test that indent rules respect `corn-ts-mode-indent-offset'."
  (let ((corn-ts-mode-indent-offset 4))
    (let ((rules (corn-ts-mode--indent-rules)))
      ;; The rules should contain the offset value (4)
      (should (cl-some (lambda (rule)
                         (and (listp rule)
                              (member 4 rule)))
                       (cdar rules)))))
  (let ((corn-ts-mode-indent-offset 8))
    (let ((rules (corn-ts-mode--indent-rules)))
      ;; The rules should contain the offset value (8)
      (should (cl-some (lambda (rule)
                         (and (listp rule)
                              (member 8 rule)))
                       (cdar rules))))))

;;;; Major Mode Tests

(ert-deftest corn-ts-mode-test-mode-defined ()
  "Test that `corn-ts-mode' is defined as a major mode."
  (should (fboundp 'corn-ts-mode)))

(ert-deftest corn-ts-mode-test-mode-activation ()
  "Test that `corn-ts-mode' can be activated (requires grammar)."
  (corn-ts-mode-test--with-grammar
    (with-temp-buffer
      (corn-ts-mode)
      (should (eq major-mode 'corn-ts-mode))
      (should (string= mode-name "Corn")))))

(ert-deftest corn-ts-mode-test-mode-derived-from-prog-mode ()
  "Test that `corn-ts-mode' is derived from `prog-mode'."
  (should (get 'corn-ts-mode 'derived-mode-parent))
  (should (eq (get 'corn-ts-mode 'derived-mode-parent) 'prog-mode)))

(ert-deftest corn-ts-mode-test-comment-settings ()
  "Test that comment settings are configured correctly when mode is active."
  (corn-ts-mode-test--with-grammar
    (with-temp-buffer
      (corn-ts-mode)
      (should (string= comment-start "// "))
      (should (string= comment-end "")))))

(ert-deftest corn-ts-mode-test-electric-indent ()
  "Test that electric indent characters are set."
  (corn-ts-mode-test--with-grammar
    (with-temp-buffer
      (corn-ts-mode)
      (should (memq ?{ electric-indent-chars))
      (should (memq ?} electric-indent-chars))
      (should (memq ?\[ electric-indent-chars))
      (should (memq ?\] electric-indent-chars)))))

(ert-deftest corn-ts-mode-test-treesit-settings ()
  "Test that tree-sitter settings are configured."
  (corn-ts-mode-test--with-grammar
    (with-temp-buffer
      (corn-ts-mode)
      ;; Font-lock settings should be set
      (should treesit-font-lock-settings)
      ;; Feature list should be set
      (should treesit-font-lock-feature-list)
      ;; Indent rules should be set
      (should treesit-simple-indent-rules)
      ;; Defun settings should be set
      (should treesit-defun-type-regexp)
      (should (eq treesit-defun-name-function #'corn-ts-mode--defun-name))
      ;; Imenu settings should be set
      (should treesit-simple-imenu-settings))))

(ert-deftest corn-ts-mode-test-font-lock-features ()
  "Test that expected font-lock features are defined."
  (corn-ts-mode-test--with-grammar
    (with-temp-buffer
      (corn-ts-mode)
      ;; Check all expected features are in the feature list
      (let ((all-features (apply #'append treesit-font-lock-feature-list)))
        (should (memq 'comment all-features))
        (should (memq 'keyword all-features))
        (should (memq 'string all-features))
        (should (memq 'constant all-features))
        (should (memq 'number all-features))
        (should (memq 'property all-features))
        (should (memq 'variable all-features))
        (should (memq 'bracket all-features))
        (should (memq 'delimiter all-features))
        (should (memq 'operator all-features))))))

;;;; Defun Name Tests

(ert-deftest corn-ts-mode-test-defun-name-function-exists ()
  "Test that the defun name function is defined."
  (should (fboundp 'corn-ts-mode--defun-name)))

;;;; Imenu Tests

(ert-deftest corn-ts-mode-test-imenu-settings ()
  "Test that imenu settings are configured."
  (corn-ts-mode-test--with-grammar
    (with-temp-buffer
      (corn-ts-mode)
      (should treesit-simple-imenu-settings)
      ;; Should have Variable and Property categories
      (should (assoc "Variable" treesit-simple-imenu-settings))
      (should (assoc "Property" treesit-simple-imenu-settings)))))

(provide 'corn-ts-mode-test)

;;; corn-ts-mode-test.el ends here
