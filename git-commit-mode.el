(defvar git-commit-mode-hook)

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

(defconst git-commit-font-lock-keywords-1
  '(("^#.*$" . 'git-commit-comment-face)
    ("\\`\\(.\\{,50\\}\\)\\(.*?\\)\n\\(.*\\)$"
     (1 'git-commit-summary-face)
     (2 'git-commit-overlong-summary-face)
     (3 'git-commit-nonempty-second-line-face))
    (".*" (0 'git-commit-text-face keep))))

(defvar git-commit-font-lock-keywords git-commit-font-lock-keywords-1)

(defun git-commit-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq font-lock-multiline t)
  (setq font-lock-defaults `(git-commit-font-lock-keywords ,t))
  (setq major-mode 'git-commit-mode)
  (run-hooks 'git-commit-mode-hook)
  (setq mode-name "Git-Commit"))

(add-to-list 'auto-mode-alist
             '("COMMIT_EDITMSG" . git-commit-mode))

(provide 'git-commit-mode)
