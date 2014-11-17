EMACS_BIN ?= emacs
EFLAGS ?=
ELS = gitattributes-mode.el gitconfig-mode.el gitignore-mode.el
ELCS = $(ELS:.el=.elc)
# These libraries used to be part of this
# repository, make sure they are gone.
ELCS += git-commit-mode.elc git-rebase-mode.elc with-editor.elc

.PHONY: lisp
lisp: $(ELCS)

.PHONY: clean
clean:
	@echo "Cleaning..."
	@rm -f $(ELCS)

%.elc: %.el
	@$(EMACS_BIN) -batch -Q -f batch-byte-compile $<
