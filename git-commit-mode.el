;;; git-commit-mode.el --- Major mode for editing git commit messages
;;; -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (c) 2012 Sebastian Wiesner <lunaryorn@gmail.com>
;; Copyright (c) 2010 Florian Ragwitz.
;;
;; Author: Florian Ragwitz <rafl@debian.org>
;;      Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/git-commit-mode
;; Version: 0.5
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

;; A major mode for editing Git commit messages.

;; Formatting
;; ----------
;;
;; Highlight the formatting of git commit messages and indicate errors according
;; to the guidelines for commit messages (see
;; http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).
;;
;; Highlight the first line (aka "summary") specially if it exceeds 54
;; characters.
;;
;; Enable `auto-fill-mode' and set the `fill-column' to 72 according to the
;; aforementioned guidelines.

;; Headers
;; -------
;;
;; Provide commands to insert standard headers into commit messages.
;;
;; C-c C-s inserts Signed-off-by (`git-commit-signoff').
;; C-C C-a inserts Acked-by (`git-commit-ack').
;; C-c C-t inserts Tested-by (`git-commit-test').
;; C-c C-r inserts Reviewed-by (`git-commit-review').
;; C-c C-o inserts Cc (`git-commit-cc').
;; C-c C-p inserts Reported-by (`git-commit-reported').

;; Committing
;; ----------
;;
;; C-c C-c finishes a commit.  By default this means to save and kill the
;; buffer.  Customize `git-commit-commit-function' to change this behaviour.

;; Magit integration
;; -----------------
;;
;; Provide optional magit integration.  To enable, add the following to our init
;; file:
;;
;; (add-hook 'magit-log-edit-mode-hook 'git-commit-mode-magit-setup)
;;
;; If enabled font lock and fill settings of `git-commit-mode' are available in
;; `magit-log-edit-mode', too.  However, the key bindings are not, because Magit
;; has it's own way of committing and dealing with headers.

;; Movement
;; --------
;;
;; `beginning-of-defun' (C-M-a) and `end-of-defun' (C-M-e) move to the beginning
;; and the end respectively of the valid, i.e. non overlong, summary line.
;;
;; These bindings are available in `magit-log-edit-mode', too, if integration is
;; configured.

;;; Code:

(require 'server)

(defgroup git-commit nil
  "Mode for editing git commit messages"
  :group 'tools)

(defgroup git-commit-faces nil
  "Faces for highlighting git commit messages"
  :prefix "git-commit-"
  :group 'git-commit
  :group 'faces)

(defface git-commit-summary-face
  '((t (:inherit font-lock-type-face)))
  "Face used to highlight the summary in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-overlong-summary-face
  '((t (:inherit font-lock-warning-face)))
  "Face used to highlight overlong parts of git commit message summaries"
  :group 'git-commit-faces)

(defface git-commit-nonempty-second-line-face
  '((t (:inherit font-lock-warning-face)))
  "Face used to highlight text on the second line of git commit messages"
  :group 'git-commit-faces)

(defface git-commit-text-face
  '((t (:inherit default)))
  "Face used to highlight text in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face used to highlight comments in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-note-face
  '((t (:inherit font-lock-string-face)))
  "Face used to highlight notes in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-pseudo-header-face
  '((t (:inherit font-lock-string-face)))
  "Font used to hightlight pseudo headers in git commit messages"
  :group 'git-commit-faces)

(defcustom git-commit-known-pseudo-headers
  '("Signed-off-by"
    "Acked-by"
    "Cc"
    "Reported-by"
    "Tested-by"
    "Reviewed-by")
  "A list of git pseudo headers to be highlighted."
  :group 'git-commit
  :type '(repeat string))

(defface git-commit-known-pseudo-header-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used to hightlight common pseudo headers in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-branch-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used to highlight the branch name in comments in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-no-branch-face
  '((t (:inherit git-commit-branch-face)))
  "Face used when a commit is going to be made outside of any branches"
  :group 'git-commit-faces)

(defface git-commit-comment-heading-face
  '((t (:inherit git-commit-known-pseudo-header-face)))
  "Face used to highlight section headings in the default
comments in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-comment-file-face
  '((t (:inherit git-commit-pseudo-header-face)))
  "Face used to highlight file names in the default comments in
git commit messages"
  :group 'git-commit-faces)

(defface git-commit-comment-action-face
  '((t (:inherit git-commit-branch-face)))
  "Face used to highlight what has happened to files in the
default comments in git commit messages"
  :group 'git-commit-faces)

(defvar git-commit-font-lock-keywords
  (append
   '(("^\\(#\s+On branch \\)\\(.*\\)$"
      (1 'git-commit-comment-face)
      (2 'git-commit-branch-face)))
   (mapcar (lambda (exp) `(,(concat "^\\(#\s+\\)\\(" (car exp) "\\)$")
                           (1 'git-commit-comment-face)
                           (2 ',(cdr exp))))
           '(("Not currently on any branch." . git-commit-no-branch-face)
             ("Changes to be committed:"     . git-commit-comment-heading-face)
             ("Untracked files:"             . git-commit-comment-heading-face)
             ("Changed but not updated:"     . git-commit-comment-heading-face)
             ("Unmerged paths:"              . git-commit-comment-heading-face)))
   `(("^\\(#\t\\)\\([^:]+\\)\\(:\s+\\)\\(.*\\)$"
      (1 'git-commit-comment-face)
      (2 'git-commit-comment-action-face)
      (3 'git-commit-comment-face)
      (4 'git-commit-comment-file-face))
     ("^\\(#\t\\)\\(.*\\)$"
      (1 'git-commit-comment-face)
      (2 'git-commit-comment-file-face))
     ("^#.*$" . 'git-commit-comment-face)
     (git-commit-find-end-of-summary-line . 'git-commit-summary-face)
     ("\\[[^\n]+?\\]" (0 'git-commit-note-face t)) ; Notes override summary line
     ;; Warnings from overlong lines and nonempty second line override
     ;; everything
     (git-commit-find-overlong-summary-line
      (0 'git-commit-overlong-summary-face t))
     (git-commit-find-nonempty-second-line
      (0 'git-commit-nonempty-second-line-face t))
     (,(concat "^\\("
               (regexp-opt git-commit-known-pseudo-headers)
               ":\\)\\(\s.*\\)$")
      (1 'git-commit-known-pseudo-header-face)
      (2 'git-commit-pseudo-header-face))
     ("^\\w[^\s\n]+:\s.*$" . 'git-commit-pseudo-header-face)
     (".*" . 'git-commit-text-face))))

(defun git-commit-end-session ()
  "Save the buffer and end the session.

If the current buffer has clients from the Emacs server, call
`server-edit' to mark the buffer as done and let the clients
continue, otherwise kill the buffer via `kill-buffer'."
  (save-buffer)
  (if server-buffer-clients
      (server-edit) ; The message buffer comes from emacsclient
    (kill-buffer)))

(defcustom git-commit-commit-function
  #'git-commit-end-session
  "Function to actually perform a commit.
Used by `git-commit-commit'."
  :group 'git-commit
  :type '(radio (function-item :doc "Save the buffer and end the session."
                               git-commit-end-session)
                (function)))

(defun git-commit-commit ()
  "Finish editing the commit message and commit.

Saves the buffer, and calls `git-commit-commit-function' to
continue.  Customize this variable as needed."
  (interactive)
  (save-buffer)
  (funcall git-commit-commit-function))

(defun git-commit-git-config-var (key)
  "Retrieve a git configuration value.
Invokes 'git config --get' to retrieve the value for the
configuration key KEY."
  (let* ((exit)
         (output
          (with-output-to-string
            (with-current-buffer
                standard-output
              (setq exit
                    (call-process "git" nil (list t nil) nil
                                  "config" "--get" key))))))
    (if (not (= 0 exit))
        nil
      (substring output 0 (- (length output) 1)))))

(defun git-commit-first-env-var (&rest vars)
  "Get the value of the first defined environment variable.
Walk VARS, call `getenv' on each element and return the first
non-nil return value of `getenv'."
  (let ((current vars)
        (val nil))
    (while (and (not val) current)
      (setq val (getenv (car current)))
      (setq current (cdr current)))
    val))

(defun git-commit-committer-name ()
  "Get the git committer name of the current user.
This uses the same mechanism git itself uses.  That is, using the
value of the 'GIT_AUTHOR_NAME' or 'GIT_COMMITTER_NAME'
environment variables, or the 'user.name' git configuration
variable.

If the above mechanism fails, the value of the variable
`user-full-name' is used."
  (or
   (git-commit-first-env-var "GIT_AUTHOR_NAME" "GIT_COMMITTER_NAME")
   (git-commit-git-config-var "user.name")
   user-full-name))

(defun git-commit-committer-email ()
  "Get the git committer email address of the current user.
This uses the same mechanism git itself uses.  That is, using the
value of the 'GIT_AUTHOR_EMAIL', 'GIT_COMMITTER_EMAIL', or
'EMAIL' environment variables, or the 'user.email' git
configuration variable.

If the above mechanism fails, the value of the variable
`user-email-address' is used."
  (or
   (git-commit-first-env-var "GIT_AUTHOR_EMAIL" "GIT_COMMITTER_EMAIL" "EMAIL")
   (git-commit-git-config-var "user.email")
   user-mail-address))

(defun git-commit-find-beginning-of-summary-line (&optional limit)
  "Find the beginning of the summary line.

Search starting at the current point up to LIMIT.  If successful,
return t and set match data to contain the whole summary line.
Otherwise return nil."
  (when
      (if (eq major-mode 'magit-log-edit-mode)
          (re-search-forward
           (format "\\`\\(?:\\(?:[A-Za-z0-9-_]+: *\\(?:.+\\)?\n\\)*%s\\)\
?\\(?:\\(?:\s*\\|#.*\\)\n\\)*"
                   (regexp-quote magit-log-header-end)) limit t)
        (re-search-forward "\\`\\(?:\\(?:\s*\\|#.*\\)\n\\)*" limit t))
    (put-text-property (match-beginning 0) (match-end 0)
                       'font-lock-multiline t)
    t))

(defun git-commit-find-end-of-summary-line (&optional limit)
  "Find the end of the summary line.

Search starting at the current point up to LIMIT.  If successful,
return t and set match data to contain the whole summary
line.  Otherwise return nil."
  (when (git-commit-find-beginning-of-summary-line limit)
    (when (re-search-forward "\\(.\\{,50\\}\\)" limit t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'font-lock-multiline t)
      t)))

(defun git-commit-find-overlong-summary-line (&optional limit)
  "Find an overlong part in the summary line.

Search starting at the current point up to LIMIT.  If successful,
return t and set match data to contain the whole summary
line.  Otherwise return nil."
  (when (git-commit-find-end-of-summary-line limit)
    (when (re-search-forward "\\(.*\\)$")
      (put-text-property (match-beginning 0) (match-end 0)
                         'font-lock-multiline t)
      t)))

(defun git-commit-find-nonempty-second-line (&optional limit)
  "Find an nonempty line immediately following the summary line.

Search starting at the current point up to LIMIT.  If successful,
return t and set match data to contain the whole summary
line.  Otherwise return nil."
  (when (git-commit-find-beginning-of-summary-line limit)
    (forward-line)
    (when (or (not limit) (<= (point) limit))
      (when (re-search-forward "^\\([^\n]*\\)$" limit t)
        (put-text-property (match-beginning 0) (match-end 0)
                           'font-lock-multiline t)
        t))))

(defun git-commit-find-pseudo-header-position ()
  "Find the position at which commit pseudo headers should be inserted.
Those headers usually live at the end of a commit message, but
before any trailing comments git or the user might have inserted."
  (save-excursion
    ;; skip the summary line, limit the search to comment region
    (let ((comment-start (point)))
      (goto-char (point-max))
      (if (not (re-search-backward "^[^#][^\s:]+:.*$" nil t))
          ;; no headers yet, so we'll search backwards for a good place
          ;; to insert them
          (if (not (re-search-backward "^[^#].*?.*$" nil t))
              ;; no comment lines anywhere before end-of-buffer, so we
              ;; want to insert right there
              (point-max)
            ;; there's some comments at the end, so we want to insert
            ;; before those
            (beginning-of-line)
            (forward-line 1)
            (point))
        ;; we're at the last header, and we want the line right after
        ;; that to insert further headers
        (beginning-of-line)
        (forward-line 1)
        (point)))))

(defun git-commit-insert-header (type name email &optional note)
  "Insert a header into the commit message.
The inserted headers have the format 'TYPE: NAME <EMAIL>'.

If NOTE satisfies `stringp', an additional note of the format
'[EMAIL: NOTE]' is inserted after the header.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted.

The header is inserted at the position returned by
`git-commit-find-pseudo-header-position'.  When this position
isn't after an existing header or a newline, an extra newline is
inserted before the header."
  (let* ((header-at (git-commit-find-pseudo-header-position))
         (prev-line (save-excursion
                      (goto-char (- header-at 1))
                      (thing-at-point 'line)))
         (pre       (if (or (string-match "^[^\s:]+:.+$" prev-line)
                            (string-match "\\`\s*$" prev-line))
                        "" "\n"))
         (insert    (lambda ()
                      (goto-char header-at)
                      (insert (format "%s%s: %s <%s>\n" pre type name email))
                      (when note
                        (insert (format "[%s: %s]\n"
                                        email (if (stringp note) note "")))
                        (backward-char 2)))))
    (if (eq t note)
        (funcall insert)
      (save-excursion (funcall insert)))))

(defun git-commit-insert-header-as-self (type &optional note)
  "Insert a header with the name and email address of the current user.
Call `git-commit-insert-header' with the user name and email
address provided by `git-commit-committer-name' and
`git-commit-committer-email'.

TYPE and NOTE are passed along unmodified."
  (let ((committer-name (git-commit-committer-name))
        (committer-email (git-commit-committer-email)))
    (git-commit-insert-header type committer-name committer-email note)))

(defmacro git-define-git-commit-self (action header)
  "Create function git-commit-ACTION.
ACTION will be part of the function name.
HEADER is the actual header to be inserted into the comment."
  (let ((func-name (intern (concat "git-commit-" action))))
    `(defun ,func-name (&optional note)
       ,(format "Insert a '%s' header at the end of the commit message.
If NOTE is given, an additional note will be inserted.

If NOTE satisfies `stringp', the value of NOTE will be inserted
as the content of the note.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted.

NOTE defaults to `current-prefix-arg'.

The author name and email address used for the header are
retrieved automatically with the same mechanism git uses."
                header)
       (interactive
        (list (when current-prefix-arg t)))
       (git-commit-insert-header-as-self ,header note))))

(git-define-git-commit-self "ack"     "Acked-by")
(git-define-git-commit-self "review"  "Reviewed-by")
(git-define-git-commit-self "signoff" "Signed-off-by")
(git-define-git-commit-self "test"    "Tested-by")

(defmacro git-define-git-commit (action header)
  "Create interactive function git-commit-ACTION.
ACTION will be part of the function name.
HEADER is the actual header to be inserted into the comment."
  (let ((func-name (intern (concat "git-commit-" action))))
    `(defun ,func-name (name email &optional note)
       ,(format "Insert a '%s' header at the end of the commit message.
The value of the header is determined by NAME and EMAIL.

When called interactively, both NAME and EMAIL are read from the
minibuffer.

If NOTE is given, an additional note will be inserted.

If NOTE satisfies `stringp', the value of NOTE will be inserted
as the content of the note.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted.

NOTE defaults to `current-prefix-arg'."
                header)
       (interactive
        (list (read-string "Name: ")
              (read-string "Email: ")
              (when current-prefix-arg t)))
       (git-commit-insert-header ,header name email note))))

(git-define-git-commit "cc" "Cc")
(git-define-git-commit "reported" "Reported-by")

(defun git-commit-beginning-of-summary-line ()
  "Move point to the beginning of the summary line."
  (interactive)
  (goto-char (point-min))
  (git-commit-find-beginning-of-summary-line))

(defun git-commit-end-of-summary-line ()
  "Move point to the end of the valid summary line.

If the summary line is overly long, i.e. exceeds 50 characters,
the point is moved to the 50th character, i.e. the end of the
valid summary line."
  (interactive)
  (goto-char (point-min))
  (when (git-commit-find-end-of-summary-line)
    (goto-char (match-end 0))))

(defvar git-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'git-commit-commit)
    (define-key map (kbd "C-c C-s") 'git-commit-signoff)
    (define-key map (kbd "C-c C-a") 'git-commit-ack)
    (define-key map (kbd "C-c C-t") 'git-commit-test)
    (define-key map (kbd "C-c C-r") 'git-commit-review)
    (define-key map (kbd "C-c C-o") 'git-commit-cc)
    (define-key map (kbd "C-c C-p") 'git-commit-reported)
    map))

(defun git-commit-font-lock-diff ()
  "Add font lock on diff."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^diff --git" nil t)
      (let ((beg (match-beginning 0)))
        (let* ((buffer (current-buffer))
               (font-lock-verbose nil)
               (font-lock-support-mode nil)
               (text (with-temp-buffer
                       (insert
                        (with-current-buffer buffer
                          (buffer-substring-no-properties beg (point-max))))
                       (diff-mode)
                       (font-lock-fontify-buffer)
                       (let ((pos (point-min))
                             next)
                         (while (setq next (next-single-property-change pos 'face))
                           (put-text-property pos next 'font-lock-face
                                              (get-text-property pos 'face))
                           (setq pos next)))
                       (buffer-string))))
          (delete-region beg (point-max))
          (insert text))))))

(defun git-commit-mode-setup-filling ()
  "Setup filling for git commit modes."
  (turn-on-auto-fill)
  (setq fill-column 72))

(defun git-commit-mode-setup-movements ()
  "Configure beginning/end of summary line movements, if possible."
  (set (make-local-variable 'beginning-of-defun-function)
       (lambda (&optional arg) (git-commit-beginning-of-summary-line)))
  (set (make-local-variable 'end-of-defun-function)
       (lambda (&optional arg) (git-commit-end-of-summary-line))))

(defun git-commit-mode-setup-font-lock (&optional default)
  "Setup font locking for git commit modes.

If DEFAULT is t, set font lock keywords as default (see
`font-lock-defaults'), otherwise just add them to the list of
keywords via `font-lock-add-keywords'."
  (if default
      (setq font-lock-defaults '(git-commit-font-lock-keywords t))
    (font-lock-add-keywords nil git-commit-font-lock-keywords))
  (git-commit-font-lock-diff))

;;;###autoload
(defun git-commit-mode-magit-setup ()
  "Setup common things for all git commit modes."
  (git-commit-mode-setup-filling)
  (git-commit-mode-setup-font-lock)
  (git-commit-mode-setup-movements))

;;;###autoload
(define-derived-mode git-commit-mode text-mode "Git Commit"
  "Major mode for editing git commit messages.

This mode helps with editing git commit messages both by
providing commands to do common tasks, and by highlighting the
basic structure of and errors in git commit messages."
  (git-commit-mode-setup-font-lock t)
  (git-commit-mode-setup-filling)
  (git-commit-mode-setup-movements)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start-skip "^#\s"
        comment-start "# "
        comment-end "")
  (when (fboundp 'toggle-save-place)
    (toggle-save-place 0)))

;;;###autoload
(when (boundp 'session-mode-disable-list)
  (add-to-list 'session-mode-disable-list 'git-commit-mode))

;;;###autoload
(setq auto-mode-alist
      (append auto-mode-alist
              '(("/COMMIT_EDITMSG\\'" . git-commit-mode)
                ("/NOTES_EDITMSG\\'" . git-commit-mode)
                ("/MERGE_MSG\\'" . git-commit-mode)
                ("/TAG_EDITMSG\\'" . git-commit-mode))))

(provide 'git-commit-mode)

;;; git-commit-mode.el ends here
