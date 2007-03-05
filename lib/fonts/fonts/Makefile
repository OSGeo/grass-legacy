
MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

SRCFONTS := $(wildcard hersh.oc?)
SRCMAPS := $(wildcard *.hmp)

DSTFONTS := $(patsubst %,$(FONTDIR)/%,$(SRCFONTS))
DSTMAPS := $(patsubst %,$(FONTDIR)/%,$(SRCMAPS))

default: $(DSTFONTS) $(DSTMAPS)

$(FONTDIR)/%: %
	$(INSTALL_DATA) $^ $(FONTDIR)
