EMACS = emacs
EFLAGS =

ELS  = git-commit-mode.el
ELS += git-rebase-mode.el
ELS += gitattributes-mode.el
ELS += gitconfig-mode.el
ELS += gitignore-mode.el
ELCS = $(ELS:.el=.elc)

.PHONY: lisp
lisp: $(ELCS)

.PHONY: clean
clean:
	@echo "Cleaning..."
	@rm -f $(ELCS)

%.elc: %.el
	@$(EMACS) $(EFLAGS) -Q -batch -f batch-byte-compile $<
