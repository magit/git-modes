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

;;;###autoload
(define-derived-mode gitconfig-mode prog-mode "Gitconfig"
  "A major mode for editing .gitconfig files."
  ;; Setup font lock
  (setq font-lock-defaults '(gitconfig-font-lock-keywords))
  ;; Configure commenting syntax
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start-skip "^#\s"
        comment-start "# "
        comment-end ""))

;;;###autoload
(setq auto-mode-alist
      (append '(("/\\.gitconfig\\'" . gitconfig-mode)
                ("/\\.git/config\\'" . gitconfig-mode))
              auto-mode-alist))

(provide 'gitconfig-mode)

;;; gitconfig-mode.el ends here
