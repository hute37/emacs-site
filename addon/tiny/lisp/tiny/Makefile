# where the url lisp files should go
prefix  = h:/bin/emacs/gnu-emacs/emacs-21.1

datadir = ${prefix}/share
lispdir = ${datadir}/emacs/site-lisp
srcdir  = .
infodir = ${prefix}/info

EMACS	= emacs
XEMACS	= xemacs
INSTALL = /usr/bin/install -c
RM      = rm -f


SUBDIRS =lisp/tiny lisp/other


.PHONY: $(SUBDIRS)

all:
	cd lisp && $(MAKE) $@

install: all
	for x in $(SUBDIRS); do ( cd $$x && $(MAKE) $@ ); done

distclean: clean
	for x in $(SUBDIRS); do ( cd $$x && $(MAKE) $@ ); done
	$(RM) config.* Makefile

clean:
	for x in $(SUBDIRS); do ( cd $$x && $(MAKE) $@ ); done

config.status: $(srcdir)/configure
	$(SHELL) ./config.status --recheck

$(srcdir)/configure: $(srcdir)/configure.in
	cd $(srcdir) && autoconf

Makefile: $(srcdir)/Makefile.in config.status
	CONFIG_FILES=$@ CONFIG_HEADERS= $(SHELL) ./config.status


# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT: