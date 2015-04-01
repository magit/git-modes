VERSION ?= 1.0.0

PREFIX  ?= /usr/local
LISPDIR ?= $(PREFIX)/share/emacs/site-lisp/git-modes

ELS  = git-commit-mode.el
ELS += git-rebase-mode.el
ELS += gitattributes-mode.el
ELS += gitconfig-mode.el
ELS += gitignore-mode.el
ELCS = $(ELS:.el=.elc)
ELMS = $(ELS:%.el=marmalade/%-$(VERSION).el)

EMACS_BIN  ?= emacs
EFLAGS ?=
BATCH   = $(EMACS_BIN) $(EFLAGS) -batch -Q -L .
BATCHE  = $(BATCH) -eval
BATCHC  = $(BATCH) -f batch-byte-compile

CP    ?= install -p -m 644
MKDIR ?= install -p -m 755 -d
RMDIR ?= rm -rf

lisp: $(ELCS)

.PHONY: install
install: lisp
	@echo "Installing..."
	@$(MKDIR) $(DESTDIR)$(LISPDIR)
	@$(CP) $(ELS) $(ELCS) $(DESTDIR)$(LISPDIR)

.PHONY: clean
clean:
	@echo "Cleaning..."
	@rm -rf $(ELCS) marmalade

%.elc: %.el
	@$(BATCHC) $<

.PHONY: test
test:
	@$(BATCHE) "(progn\
	(require 'cl) \
	(put 'flet 'byte-obsolete-info nil))" \
	-l tests/git-commit-tests.el -f ert-run-tests-batch-and-exit

.PHONY: test-interactive
test-interactive:
	@$(EMACS) $(EFLAGS) -Q -L "." --eval "(progn\
	(require 'cl)\
	(put 'flet 'byte-obsolete-info nil)\
	(load-file \"tests/git-commit-tests.el\")\
	(ert t))"

.PHONY: marmalade-upload
marmalade-upload: marmalade
	@marmalade-upload $(ELMS)
	@rm -rf marmalade
marmalade: $(ELMS)
$(ELMS): marmalade/%-$(VERSION).el: %.el
	@echo $< $@
	@mkdir -p marmalade
	@cp $< $@
	@sed -e "/^;; Keywords:/a;; Package-Version: $(VERSION)" -i $@
