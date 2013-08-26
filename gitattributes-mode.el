;;; gitattributes-mode --- Major mode for editing .gitattributes files -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Created: 26 Aug 2013
;; Version: 0.14.0
;; Homepage: https://github.com/magit/git-modes
;; Keywords: convenience vc git

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing gitattributes(5) files.

;;; Format:

;; pattern attr1 attr2 ... attrN

;; Pattern format is described in gitignore(5).

;;; Code:

(defgroup gitattributes-mode nil
  "Major mode for editing .gitattributes files"
  :link '(url-link "https://github.com/magit/git-modes")
  :prefix "gitattributes-mode-"
  :group 'tools)

(defcustom gitattributes-mode-man-function #'man
  "Function to open the gitattributes(5) manpage."
  :type '(choice (const :tag "Man" #'man)
                 (const :tag "Woman" #'woman)
                 (function :tag "Function"))
  :group 'gitattributes-mode)

(defun gitattributes-mode-help ()
  "Open the gitattributes(5) manpage using `gitattributes-mode-man-function'."
  (interactive)
  (funcall gitattributes-mode-man-function "gitattributes(5)"))

(defvar gitattributes-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?- ".w" table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?= "." table)
    table)
  "Syntax table for `gitattributes-mode'.")

(defvar gitattributes-mode-font-lock-keywords
  ;; TODO use `gitignore-mode-font-lock-keywords' for first column.
  '(("\\(?:\\(?:fals\\|tru\\)e\\)" . 'font-lock-keyword-face)
    ("\\s-+\\(-\\|!\\)[[:word:]]+" 1 'font-lock-negation-char-face)
    ("\\s-+\\(?:-\\|!\\)?\\([[:word:]]+\\)=?" 1 'font-lock-variable-name-face))
  "Keywords for highlight in `gitattributes-mode'.")

(defun gitattributes-mode-forward-field (&optional arg)
  "Move point ARG fields forward.
If ARG is omitted or nil, move point forward one field."
  (interactive "p")
  (if (< arg 0)
      (gitattributes-mode-backward-field (- arg))
    (dotimes (_ (or arg 1))
      (re-search-forward "\\s-" nil 'move))))

(defun gitattributes-mode-backward-field (&optional arg)
  "Move point ARG fields forward.
If ARG is omitted or nil, move point forward one field."
  (interactive "p")
  (if (< arg 0)
      (gitattributes-mode-forward-field (- arg))
    (dotimes (_ (or arg 1))
      (re-search-backward "\\s-" nil 'move))))

(defvar gitattributes-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `gitattributes-mode'.")

(easy-menu-define gitattributes-mode-menu
  gitattributes-mode-map
  "Menu for `gitattributes-mode'."
  '("Git Attributes"
    ["Forward Field" forward-sexp :active t
     :help "Move forward across one field"]
    ["Backward Field" backward-sexp :active t
     :help "Move backward across one field"]
    ["Kill Field Forward" kill-sexp :active t
     :help "Kill field following cursor"]
    ["Kill Field Backward" backward-kill-sexp :active t
     :help "Kill field preceding cursor"]
    "--"
    ["Help" gitattributes-mode-help :active t
     :help "Open gitattributes(5) manpage"]))

;;;###autoload
(define-derived-mode gitattributes-mode text-mode "Gitattributes"
  "A major mode for editing .gitattributes files.
\\{gitattributes-mode-map}"
  :group 'gitattributes-mode
  :syntax-table gitattributes-mode-syntax-table
  (setq font-lock-defaults '(gitattributes-mode-font-lock-keywords))
  (setq-local forward-sexp-function #'gitattributes-mode-forward-field))

;;;###autoload
(dolist (pattern '("/\\.gitattributes\\'"
                   "/\\.git/info/attributes\\'"
                   "/git/attributes\\'"))
  (add-to-list 'auto-mode-alist (cons pattern #'gitattributes-mode)))

(provide 'gitattributes-mode)

;;; gitattributes-mode.el ends here
