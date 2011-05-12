# some definitions
SECT = 1
MANDIR  = $(ARCH_DISTDIR)/man/man$(SECT)
HTMLDIR = $(ARCH_DISTDIR)/docs/html
HTML2MAN = GRASS_PERL=${PERL} VERSION_NUMBER=${GRASS_VERSION_NUMBER} sh $(MODULE_TOPDIR)/tools/g.html2man/g.html2man

# generic man rules for all commands

ifdef CROSS_COMPILING

mancmd:

manscript:

manetc:

mandir:

manmulti:

else

ifeq ($(PERL),no)

mancmd:

manscript:

manetc:

mandir:

manmulti:

else

$(MANDIR)/$(PGM).${SECT}: $(MANSRC)
	$(MKDIR) $(MANDIR)
	$(HTML2MAN) $< $@ $(SECT)

# man rules for cmd commands
mancmd:
	$(MAKE) $(MANDIR)/$(PGM).${SECT} MANSRC=$(HTMLDIR)/$(PGM).html

# man rules for scripts
manscript:
	$(MAKE) $(MANDIR)/$(PGM).${SECT} MANSRC=$(HTMLDIR)/$(PGM).html

# man rules for ETC commands
manetc:
	$(MAKE) $(MANDIR)/$(PGM).${SECT} MANSRC=$(HTMLDIR)/$(PGM).html

# man rules for intro pages in directories
mandir:
	$(MAKE) $(MANDIR)/$(PGM).${SECT} MANSRC=$(HTMLDIR)/$(PGM).html

# man rules for multiple commands
manmulti:
	for prog in $(PROGRAMS) ; do $(MAKE) mancmd PGM=$$prog ; done

endif

endif

.PHONY: mancmd manetc manscript mandir manmulti
