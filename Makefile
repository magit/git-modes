EMACS_BIN  ?= emacs
EFLAGS ?= -L ../dash
BATCH   = $(EMACS_BIN) $(EFLAGS) -batch -Q -L .
BATCHE  = $(BATCH) -eval
BATCHC  = $(BATCH) -f batch-byte-compile

ELS  = git-commit-mode.el
ELS += git-rebase-mode.el
ELS += gitattributes-mode.el
ELS += gitconfig-mode.el
ELS += gitignore-mode.el
ELS += with-editor.el
ELCS = $(ELS:.el=.elc)

.PHONY: lisp
lisp: $(ELCS)

.PHONY: clean
clean:
	@echo "Cleaning..."
	@rm -f $(ELCS)

%.elc: %.el
	@$(BATCHC) $<
