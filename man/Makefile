MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Dir.make

# some definitions
SECT = 1
MANDIR  = $(GISBASE)/man/man$(SECT)
HTMLDIR = $(GISBASE)/docs/html
HTML2MAN = GRASS_PERL=${PERL} VERSION_NUMBER=${GRASS_VERSION_NUMBER} sh $(GRASS_HOME)/tools/g.html2man/g.html2man

MANPAGES := $(patsubst $(HTMLDIR)/%.html,$(MANDIR)/%.$(SECT),$(wildcard $(HTMLDIR)/*.html))

default: $(MANDIR) $(MANPAGES)

$(MANDIR):
	$(MKDIR) $(MANDIR)

$(MANDIR)/%.$(SECT): $(HTMLDIR)/%.html
	$(HTML2MAN) $< $@ $(SECT)

