;;; gitconfig-mode.el --- Major mode for editing .gitconfig files
;;; -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (c) 2012 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/gitconfig-mode
;; Version: 0.2
;; Keywords: convenience vc git

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; A major mode for editing .gitconfig files.

;;; Code:

(require 'conf-mode)

(defun gitconfig-line-indented-p ()
  "Determine whether the current line is intended correctly.

Return t if so, or nil otherwise."
  (save-excursion
    (beginning-of-line)
    (or (looking-at "^\\[\\_<.*?\\]")
        (looking-at "^\t\\_<\\(?:\\sw|\\s_\\)"))))

(defun gitconfig-indent-line ()
  "Indent the current line."
  (interactive)
  (unless (gitconfig-line-indented-p)
    (let ((point-in-line (point)))
      (back-to-indentation)
      (setq point-in-line (- point-in-line
                             (- (point) (line-beginning-position))))
      (beginning-of-line)
      (delete-horizontal-space)
      (unless (= (char-after) ?\[)
        (insert-tab)
        (setq point-in-line (+ point-in-line 1)))
      (if (>= point-in-line (line-beginning-position))
          (goto-char point-in-line)
        (back-to-indentation)))))

(defvar gitconfig-mode-syntax-table
  (let ((table (make-syntax-table conf-unix-mode-syntax-table)))
    ;; ; is a comment in .gitconfig
    (modify-syntax-entry ?\; "<" table)
    table)
  "Syntax table to use in .gitconfig buffers.")

(defvar gitconfig-font-lock-keywords
  `(
    ;; Highlight section and subsection gitconfig headers, and override
    ;; syntactic fontification in these.
    ("^\\s-*\\[\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>\\(?:\\s-+\\(\\s\".+?\\s\"\\)\\)?\\]\\s-*"
     (1 'font-lock-type-face t nil)
     (2 'font-lock-function-name-face t t))
    ("^\\s-*\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>\\s-*\\(?:=.*\\)?$"
     (1 'font-lock-variable-name-face))
    ;; Highlight booleans and numbers
    (,(format "=\\s-*%s\\s-*$"
              (regexp-opt '("yes" "no" "true" "false" "on" "off") 'words))
     (1 'font-lock-keyword-face))
    ("=\\s-*\\<\\([0-9]+\\)\\>\\s-*$" (1 'font-lock-constant-face))))

;;;###autoload
(define-derived-mode gitconfig-mode conf-unix-mode "Gitconfig"
  "A major mode for editing .gitconfig files."
  ;; .gitconfig is indented with tabs only
  (conf-mode-initialize "#" gitconfig-font-lock-keywords)
  (setq indent-tabs-mode t)
  (set (make-local-variable 'indent-line-function)
       'gitconfig-indent-line))

;;;###autoload
(setq auto-mode-alist
      (append '(("/\\.gitconfig\\'" . gitconfig-mode)
                ("/\\.git/config\\'" . gitconfig-mode))
              auto-mode-alist))

(provide 'gitconfig-mode)

;;; gitconfig-mode.el ends here
