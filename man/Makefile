MODULE_TOPDIR = ..

# some definitions
SECT = 1
MANDIR  = $(GISBASE)/man/man$(SECT)
HTMLDIR = $(GISBASE)/docs/html
HTML2MAN = GRASS_PERL=${PERL} VERSION_NUMBER=${GRASS_VERSION_NUMBER} sh $(GRASS_HOME)/tools/g.html2man/g.html2man

MANPAGES := $(patsubst $(HTMLDIR)/%.html,$(MANDIR)/%.$(SECT),$(wildcard $(HTMLDIR)/*.html))

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: $(MANPAGES)

$(MANDIR):
	$(MKDIR) $(MANDIR)

$(MANDIR)/%.$(SECT): $(HTMLDIR)/%.html | $(MANDIR)
	$(HTML2MAN) $< $@ $(SECT)

