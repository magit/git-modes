;;; git-commit.el --- Major mode for editing git commit messages

;; Copyright (C) 2010 Florian Ragwitz

;; Author: Florian Ragwitz <rafl@debian.org>
;; Version: 0.0
;; Keywords: git

;;; Commentary:
;;

;;; Code:

(defgroup git-commit '((jit-lock custom-group))
  "Mode for editing git commit messages"
  :group 'faces)

(defgroup git-commit-faces nil
  "Faces for highlighting git commit messages"
  :prefix "git-commit-"
  :group 'git-commit)

(defface git-commit-summary-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark)) (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight the summary in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-overlong-summary-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Red1" :weight bold))
    (((class color) (min-colors 88) (background dark)) (:foreground "Pink" :weight bold))
    (((class color) (min-colors 16) (background light)) (:foreground "Red1" :weight bold))
    (((class color) (min-colors 16) (background dark)) (:foreground "Pink" :weight bold))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:inverse-video t :weight bold)))
  "Face used to highlight overlong parts of git commit message summaries"
  :group 'git-commit-faces)

(defface git-commit-nonempty-second-line-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Red1" :weight bold))
    (((class color) (min-colors 88) (background dark)) (:foreground "Pink" :weight bold))
    (((class color) (min-colors 16) (background light)) (:foreground "Red1" :weight bold))
    (((class color) (min-colors 16) (background dark)) (:foreground "Pink" :weight bold))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:inverse-video t :weight bold)))
  "Face used to highlight text on the second line of git commit messages"
  :group 'git-commit-faces)

(defface git-commit-text-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark)) (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight text in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-comment-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "chocolate1"))
    (((class color) (min-colors 16) (background light))
     (:foreground "red"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "red1"))
    (((class color) (min-colors 8) (background light))
     (:foreground "red"))
    (((class color) (min-colors 8) (background dark)))
    (t (:weight bold :slant italic)))
  "Face used to highlight comments in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-pseudo-header-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Font used to hightlight pseudo headers in git commit messages"
  :group 'git-commit-faces)

(defvar git-commit-known-pseudo-headers
  '("Signed-off-by"
    "Acked-by"
    "Cc"
    "Reported-by"
    "Tested-by"
    "Reviewed-by"))

(defface git-commit-known-pseudo-header-face
  '((((class grayscale) (background light)) (:foreground "Gray90" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:weight bold :underline t)))
  "Face used to hightlight common pseudo headers in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-note-brace-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :weight bold :underline t))
    (((class color) (min-colors 88) (background light)) (:foreground "dark cyan"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 8)) (:foreground "magenta"))
    (t (:weight bold :underline t)))
  "Face used to highlight braces within notes in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-note-address-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Face used to highlight email addresses within notes in git commit messages"
  :group 'git-commit-faces)

(defface git-commit-note-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark)) (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight notes within git commit messages"
  :group 'git-commit-faces)

(defface git-commit-branch-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark)) (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight the branch name in comments in git commit messages"
  :group 'git-commit-faces)

;; TODO:
;;  * "Not currently on any branch"
;;  * modified/untracked/staged/etc files
(defconst git-commit-font-lock-keywords-1
  `(("^\\(#\s+On branch \\)\\(.*\\)$"
     (1 'git-commit-comment-face)
     (2 'git-commit-branch-face))
    ("^#.*$"
     (0 'git-commit-comment-face))
    ("\\`\\(.\\{,50\\}\\)\\(.*?\\)\n\\(.*\\)$"
     (1 'git-commit-summary-face)
     (2 'git-commit-overlong-summary-face)
     (3 'git-commit-nonempty-second-line-face))
    (,(concat "^\\(" (regexp-opt git-commit-known-pseudo-headers) ":\\)\\(\s.*\\)$")
     (1 'git-commit-known-pseudo-header-face)
     (2 'git-commit-pseudo-header-face))
    ("^\\w[^\s\n]+:\s.*$"
     (0 'git-commit-pseudo-header-face))
    ("^\\(\\[\\)\\([^\s@]+@[^\s@]+:\\)\\(.*\\)\\(\\]\\)$"
     (1 'git-commit-note-brace-face)
     (2 'git-commit-note-address-face)
     (3 'git-commit-note-face)
     (4 'git-commit-note-brace-face))
    (".*"
     (0 'git-commit-text-face))))

(defvar git-commit-font-lock-keywords git-commit-font-lock-keywords-1)

(defvar git-commit-mode-hook nil
  "List of functions to be called when activating `git-commit-mode'.")

(defvar git-commit-commit-hook nil
  "List of functions to be called on `git-commit-commit'.")

(defun git-commit-commit ()
  "Finish editing the commit message and commit.
By default this only calls `save-buffer', as there is no general
way to actually trigger git to commit whatever the commit message
was intended for.

After calling `save-buffer', the hooks in
`git-commit-commit-hook' will be run. If you have configured git
in a way that simply invokes emacs for editing the commit
message, you might want to this:

  (add-hook 'git-commit-commit-hook
          (lambda () (save-buffers-kill-terminal)))"
  (interactive)
  (save-buffer)
  (run-hooks 'git-commit-commit-hook))

(defun git-commit-git-config-var (var)
  (let* ((exit)
        (output
         (with-output-to-string
           (with-current-buffer
               standard-output
             (setq exit
                   (call-process "git" nil (list t nil) nil "config" "--get" var))))))
    (if (not (= 0 exit))
        nil
      (substring output 0 (- (length output) 1)))))

(defun git-commit-first-env-var (&rest vars)
  ;; this is horrible. i should figure out enough elisp to make it
  ;; slightly less horrible.
  (let ((res)
        (i vars))
    (while (and (not res) i)
      (setq res (getenv (car i)))
      (setq i (cdr i)))
    res))

(defun git-commit-comitter-name ()
  (or
   (git-commit-first-env-var "GIT_AUTHOR_NAME" "GIT_COMMITTER_NAME")
   (git-commit-git-config-var "user.name")
   user-full-name))

(defun git-commit-comitter-email ()
  (or
   (git-commit-first-env-var "GIT_AUTHOR_EMAIL" "GIT_COMMITTER_EMAIL" "EMAIL")
   (git-commit-git-config-var "user.email")
   user-email-address))

(defun git-commit-find-pseudo-header-position ()
  (save-excursion
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
      (point))))

(defun git-commit-insert-header (type name email &optional note)
  (let* ((header-at (git-commit-find-pseudo-header-position))
         (prev-line (save-excursion
                      (goto-char (- signoff-at 1))
                      (thing-at-point 'line)))
         (pre       (if (or (string-match "^[^\s:]+:.+$" prev-line)
                            (string-match "\\`\s*$" prev-line))
                        "" "\n"))
         (insert    (lambda ()
                      (goto-char signoff-at)
                      (insert (format "%s%s: %s <%s>\n" pre type name email))
                      (when note
                        (insert (format "[%s: %s]\n"
                                        email (if (stringp note) note "")))
                        (backward-char 2)))))
    (if (eq t note)
        (funcall insert)
      (save-excursion (funcall insert)))))

(defun git-commit-insert-header-as-self (type &optional note)
  (let ((comitter-name (git-commit-comitter-name))
        (comitter-email (git-commit-comitter-email)))
    (git-commit-insert-header type comitter-name comitter-email note)))

(defun git-commit-signoff (&optional note)
  "Insert a 'Signed-off-by' header at the end of the commit message.
If NOTE is given, an additional note will be inserted.

If NOTE satisfies `stringp', the value of NOTE will be inserted
as the content of the note.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted."
  (interactive
   (list (when current-prefix-arg t)))
  (git-commit-insert-header-as-self "Signed-off-by" note))

(defun git-commit-ack (&optional note)
  (interactive
   (list (when current-prefix-arg t)))
  (git-commit-insert-header-as-self "Acked-by" note))

(defun git-commit-test (&optional note)
  (interactive
   (list (when current-prefix-arg t)))
  (git-commit-insert-header-as-self "Tested-by" note))

(defun git-commit-review (&optional note)
  (interactive
   (list (when current-prefix-arg t)))
  (git-commit-insert-header-as-self "Reviewed-by" note))

(defun git-commit-cc (name email &optional note)
  (interactive
   (list (read-string "Name: ")
         (read-string "Email: ")
         (when current-prefix-arg t)))
  (git-commit-insert-header "Cc" name email note))

(defun git-commit-reported (name email &optional note)
  (interactive
   (list (read-string "Name: ")
         (read-string "Email: ")
         (when current-prefix-arg t)))
  (git-commit-insert-header "Reported-by" name email note))

(defvar git-commit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'git-commit-commit)
    (define-key map (kbd "C-c C-s") 'git-commit-signoff)
    (define-key map (kbd "C-c C-a") 'git-commit-ack)
    (define-key map (kbd "C-c C-t") 'git-commit-test)
    (define-key map (kbd "C-c C-r") 'git-commit-review)
    (define-key map (kbd "C-c C-o") 'git-commit-cc)
    (define-key map (kbd "C-c C-p") 'git-commit-reported)
    map))

(defun git-commit-mode ()
  "Major mode for editing git commit messages."
  (interactive)
  (kill-all-local-variables)
  (use-local-map git-commit-map)
  (setq font-lock-multiline t)
  (setq font-lock-defaults '(git-commit-font-lock-keywords t))
  (setq major-mode 'git-commit)
  (run-hooks 'git-commit-mode-hook)
  (setq mode-name "Git-Commit"))

(add-to-list 'auto-mode-alist
             '("COMMIT_EDITMSG" . git-commit-mode))

(provide 'git-commit)

;;; git-commit.el ends here
