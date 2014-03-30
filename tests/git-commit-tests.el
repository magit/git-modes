;;; git-commit-tests.el --- tests for `git-commit-mode.el'

;; Copyright (C) 2013-2014  The Magit Project Developers
;;
;; License: GPLv3

;;; Code:

(require 'cl-lib)
(require 'ert)

(require 'git-commit-mode)

;;; Utilities

(defmacro git-commit--silentio (&rest body)
  ;; Once upon a time there was a dynamic `flet'...
  (declare (indent defun))
  (let ((orig (cl-gensym)))
    `(let ((,orig (symbol-function 'message)))
       (fset 'message (lambda (&rest silentio)))
       (prog1 (progn ,@body)
	 (fset 'message ,orig)))))

(defmacro git-commit-with-temp-buffer (&rest body)
  "Like `with-temp-buffer', but put the buffer in `git-commit' mode."
  (declare (indent defun))
  `(with-temp-buffer
     (git-commit-mode)
     (erase-buffer)                   ; `git-commit-mode' adds newline
     (remove-hook 'kill-buffer-query-functions
                  'with-editor-kill-buffer-noop t)
     ,@body))

(defmacro git-commit-message-end-position-test (test-name msg comment)
  "Make a `git-commit-message-end-position' test."
  (declare (indent defun))
  `(ert-deftest ,(intern (concat "git-commit-message-end-position-test-"
                                 (symbol-name test-name))) ()
     ,(format "Test `git-commit-message-end-position': %s" test-name)
     (git-commit-with-temp-buffer
       (insert ,msg ,comment)
       (should (equal (buffer-substring
                       1 (git-commit-find-pseudo-header-position))
                      ,msg)))))

(defmacro git-commit-with-temp-message-history (&rest body)
  `(let ((log-edit-comment-ring (make-ring log-edit-maximum-comment-ring-size))
         (log-edit-comment-ring-index nil))
    (ring-insert log-edit-comment-ring "msg one\n\n")
    (ring-insert log-edit-comment-ring "msg two\n\n")
    (ring-insert log-edit-comment-ring "msg three\n\n")
    (git-commit-with-temp-buffer (git-commit--silentio ,@body))))

;;; Tests

(git-commit-message-end-position-test msg+nl+comment
  "msg\n\n" "#comment\n")
(git-commit-message-end-position-test msg+comment
  "msg\n" "#comment\n")
(git-commit-message-end-position-test msg+2comment
  "msg\n\n" "#comment\n\n#more comment\n")
(git-commit-message-end-position-test nl+comment
  "\n\n" "#comment\n\n#more comment\n")

(ert-deftest git-commit-message-history-leave-comments ()
  "History cycling commands should not affect comments"
  (git-commit-with-temp-message-history
   (insert "current msg\n\n#comment")
   (git-commit-prev-message 1)
   (should (equal (buffer-string) "msg three\n\n#comment"))))

(ert-deftest git-commit-message-history-leave-comments-empty ()
  "History cycling commands should not affect comments, start from empty message."
  (git-commit-with-temp-message-history
   (insert "\n\n#comment")
   (git-commit-prev-message 1)
   (should (equal (buffer-string) "msg three\n\n#comment"))))

(provide 'git-commit-tests)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; git-commit-tests.el ends here
