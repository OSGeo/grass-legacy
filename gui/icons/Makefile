MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Dir.make

SRCICO := $(wildcard *.ico *.png)
SRCICONS := $(wildcard grass/*.gif grass/vdigit/*.gif grass/vdigit/*.xbm silk/*.png grass2/*.png)
ETCICONS := $(patsubst %,$(ETC)/gui/icons/%,$(SRCICONS) $(SRCICO))
HTMLICONS := $(patsubst grass2/%,$(HTMLDIR)/icons/%,$(wildcard grass2/*.png))
HTMLICONS += $(patsubst grass/%,$(HTMLDIR)/icons/%,$(wildcard grass/*.gif))

ETCDIRS = \
	$(ETC)/gui \
	$(ETC)/gui/icons/grass \
	$(ETC)/gui/icons/grass/vdigit \
	$(ETC)/gui/icons/silk \
	$(ETC)/gui/icons/grass2

HTMLDIRS = \
	$(HTMLDIR)/icons

default: $(ETCICONS) $(HTMLICONS)

$(ETC)/gui/icons/%: % | $(ETCDIRS)
	$(INSTALL_DATA) $< $@

$(HTMLDIR)/icons/%: grass/% | $(HTMLDIRS)
	$(INSTALL_DATA) $< $@

$(HTMLDIR)/icons/%: grass2/% | $(HTMLDIRS)
	$(INSTALL_DATA) $< $@

$(ETCDIRS) $(HTMLDIRS): %:
	$(MKDIR) $@
