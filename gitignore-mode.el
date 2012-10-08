;;; gitignore-mode.el --- Major mode for editing .gitconfig files
;;; -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (c) 2012 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/git-modes
;; Version: 0.1
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

;; A major mode for editing .gitignore files.

;;; Code:

(require 'conf-mode)

(defvar gitignore-mode-font-lock-keywords
  '(("^\\s<.*$" . 'font-lock-comment-face)
    ("^\\(!?\\)" (1 'font-lock-negation-char-face)) ; Negated patterns
    ("/" . 'font-lock-constant-face)                ; Directory separators
    ("\\(?:\\*\\|\\?\\)" . 'font-lock-keyword-face) ; Glob patterns
    ("\\[.+?\\]" . 'font-lock-keyword-face)         ; Ranged glob patterns
    ))

;;;###autoload
(define-derived-mode gitignore-mode conf-unix-mode "Gitignore"
  "A major mode for editing .gitconfig files."
  (conf-mode-initialize "#")
  ;; Disable syntactic font locking, because comments are only valid at
  ;; beginning of line.
  (setq font-lock-defaults '(gitignore-mode-font-lock-keywords t t))
  (set (make-local-variable 'conf-assignment-sign) nil))

;;;###autoload
(setq auto-mode-alist
      (append '(("/\\.gitignore\\'" . gitignore-mode)
                ("/\\.git/info/exclude\\'" . gitignore-mode))
              auto-mode-alist))

(provide 'gitignore-mode)

;;; gitignore-mode.el ends here
