
OBJARCH = $(subst $(SRCDIR),$(DSTDIR),$(CURDIR))

MAKEALL = for dir in $(wildcard *) ; do \
	if test -f $$dir/makefile ; then \
		$(MAKE) -f makefile -C $$dir dir || exit 1; \
	else true ; fi ; done

GMAKE = $(MAKE) -f makefile dir -C

