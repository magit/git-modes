EMACS_BIN ?= emacs
EFLAGS ?=
ELS = gitattributes-mode.el gitconfig-mode.el gitignore-mode.el
ELCS = $(ELS:.el=.elc)
ELCS_OLD = git-commit-mode.elc git-rebase-mode.elc with-editor.elc

.PHONY: lisp
lisp: $(ELCS)

.PHONY: clean
clean:
	@echo "Cleaning..."
	@rm -f $(ELCS) $(ELCS_OLD)

%.elc: %.el
	@$(EMACS_BIN) -batch -Q -f batch-byte-compile $<
