(require 'cl)
(require 'ert)

(require 'git-commit-mode)

;;; tests


(defmacro git-commit-message-end-position-test (test-name msg comment)
  "Make a `git-commit-message-end-position' test."
  (declare (indent defun))
  `(ert-deftest ,(intern (concat "git-commit-message-end-position-test-"
                                 (symbol-name test-name))) ()
     (concat "Test `git-commit-message-end-position': " ,(symbol-name test-name))
     (with-temp-buffer
       (insert ,msg ,comment)
       (should (equal (buffer-substring
                       1 (git-commit-find-pseudo-header-position))
                      ,msg)))))

(git-commit-message-end-position-test msg+nl+comment
  "msg\n\n" "#comment\n")
(git-commit-message-end-position-test msg+comment
  "msg\n" "#comment\n")
(git-commit-message-end-position-test msg+2comment
  "msg\n\n" "#comment\n\n#more comment\n")
(git-commit-message-end-position-test nl+comment
  "\n\n" "#comment\n\n#more comment\n")
