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
  "Font used to hightlight common pseudo headers in git commit messages"
  :group 'git-commit-faces)

(defconst git-commit-font-lock-keywords-1
  `(("^#.*$"
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
    (".*"
     (0 'git-commit-text-face))))

(defvar git-commit-font-lock-keywords git-commit-font-lock-keywords-1)

(defvar git-commit-mode-hook)
(defvar git-commit-commit-hook)

(defun git-commit-commit ()
  (interactive)
  (save-buffer)
  (run-hooks 'git-commit-commit-hook))

(defun git-commit-signoff ()
  (interactive)
  (let ((comitter-name "foo")
        (comitter-email "bar"))
    (save-excursion
      (end-of-buffer)
      (if (not (re-search-backward "^[^#]" nil t))
          (beginning-of-buffer)
        (end-of-line)
        )
      (insert (format "\nSigned-off-by: %s <%s>\n" comitter-name comitter-email)))))

(defvar git-commit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'git-commit-commit)
    (define-key map (kbd "C-c C-s") 'git-commit-signoff)
    map))

(defun git-commit-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map git-commit-map)
  (setq font-lock-multiline t)
  (setq font-lock-defaults `(git-commit-font-lock-keywords ,t))
  (setq major-mode 'git-commit-mode)
  (run-hooks 'git-commit-mode-hook)
  (setq mode-name "Git-Commit"))

(add-to-list 'auto-mode-alist
             '("COMMIT_EDITMSG" . git-commit-mode))

(provide 'git-commit-mode)
