;;; gitconfig-mode.el --- Major mode for editing .gitconfig files
;;; -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (c) 2012 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/gitconfig-mode
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

;; A major mode for editing .gitconfig files.

;;; Code:

(defgroup gitconfig nil
  "Mode for editing git config files"
  :group 'tools)

(defgroup gitconfig-faces nil
  "Faces for highlighting git config files"
  :prefix "gitconfig-"
  :group 'gitconfig
  :group 'faces)

(defface gitconfig-section-name-face
  '((t (:inherit font-lock-function-name-face)))
  "Face used to highlight the name of a section."
  :group 'gitconfig-faces)

(defface gitconfig-key-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used to highlight the name of a key."
  :group 'gitconfig-faces)

(defface gitconfig-value-face
  '((t (:inherit font-lock-string-face)))
  "Face used to highlight the value of a key."
  :group 'gitconfig-faces)

(defvar gitconfig-font-lock-keywords
  '(("^\s*\\[\\(.*\\)\\]\s*$" (1 'gitconfig-section-name-face))
    ("^\s*\\(.+?\\)\s*=\s*\\(.+\\)\s*$"
     (1 'gitconfig-key-face)
     (2 'gitconfig-value-face))))

(defun gitconfig-beginning-of-section (&optional arg)
  "Move point backwards to the beginning of a section.

With ARG, do it that many times.  Negative ARG means move forward
to the ARGth following beginning of a section.

Return t if the movement was successful, or nil otherwise."
  (interactive "^p")
  (let ((repeat (or arg 1)))
    (when (re-search-backward "^\s*\\(\\[.*\\]\\)\s*$" nil t repeat)
      (goto-char (match-beginning 1))
      t)))

(defun gitconfig-end-of-section (&optional arg)
  "Move point forward to next end of a section.

With ARG, to it that many times.  Negative ARG means move
backwards to the ARGth preceding end of a section.

Return t if the movement was successful, or nil otherwise."
  (interactive "^p")
  (let ((repeat (or arg 1)))
    (when (re-search-forward "\\(.\\)\n\s*\\[.*\\]\s*$" nil t repeat)
      (goto-char (match-end 1))
      t)))

(defun gitconfig-indent-line ()
  "Indent the current line."
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (unless (looking-at "^\\[.*\\]\s*$")
    (insert-tab)))

;;;###autoload
(define-derived-mode gitconfig-mode prog-mode "Gitconfig"
  "A major mode for editing .gitconfig files."
  ;; Setup font lock
  (setq font-lock-defaults '(gitconfig-font-lock-keywords))
  ;; Configure indentation
  (setq indent-tabs-mode t)
  ;; Configure commenting syntax
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start-skip "^#\s"
        comment-start "# "
        comment-end "")
  ;; Section movement
  (set (make-local-variable 'beginning-of-defun-function)
       'gitconfig-beginning-of-section)
  (set (make-local-variable 'end-of-defun-function)
       'gitconfig-end-of-section)
  ;; Indentation
  (set (make-local-variable 'indent-line-function)
       'gitconfig-indent-line))

(modify-syntax-entry ?# "<" gitconfig-mode-syntax-table)
(modify-syntax-entry ?\n ">" gitconfig-mode-syntax-table)

;;;###autoload
(setq auto-mode-alist
      (append '(("/\\.gitconfig\\'" . gitconfig-mode)
                ("/\\.git/config\\'" . gitconfig-mode))
              auto-mode-alist))

(provide 'gitconfig-mode)

;;; gitconfig-mode.el ends here
