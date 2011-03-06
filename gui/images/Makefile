MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

ETCDIR = $(ETC)/gui/images

FILES := $(wildcard *.png)
FILES += $(wildcard *.gif)

default: $(patsubst %,$(ETCDIR)/%,$(FILES))

$(ETCDIR)/%.png: %.png | $(ETCDIR)
	$(INSTALL_DATA) $< $@

$(ETCDIR)/%.gif: %.gif | $(ETCDIR)
	$(INSTALL_DATA) $< $@

$(ETCDIR):
	$(MKDIR) $@
