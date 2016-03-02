PREFIX  ?= /usr/local
LISPDIR ?= $(PREFIX)/share/emacs/site-lisp/git-modes

ELS  = git-modes.el
ELS += gitattributes-mode.el
ELS += gitconfig-mode.el
ELS += gitignore-mode.el
ELCS = $(ELS:.el=.elc)

EMACS_BIN ?= emacs

CP    ?= install -p -m 644
MKDIR ?= install -p -m 755 -d
RMDIR ?= rm -rf
SED   ?= sed

VERSION ?= $(shell test -e .git && git describe --tags --dirty 2> /dev/null)
ifeq "$(VERSION)" ""
  VERSION = 1.2.0
endif

.PHONY: install clean

lisp: $(ELCS)

install: lisp
	@printf "Installing...\n"
	@$(MKDIR) $(DESTDIR)$(LISPDIR)
	@$(CP) $(ELS) $(ELCS) $(DESTDIR)$(LISPDIR)

clean:
	@printf "Cleaning...\n"
	@$(RM) $(ELCS)

%.elc: %.el
	@printf "Compiling $<...\n"
	@$(EMACS_BIN) -batch -Q -L . -f batch-byte-compile $<
