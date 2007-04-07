MODULE_TOPDIR = ../..

PGM = d.text.freetype
CAPFILE=freetypecap

include $(MODULE_TOPDIR)/include/Make/Script.make

default: script $(ETC)/$(CAPFILE)

$(ETC)/$(CAPFILE): $(CAPFILE)
	$(INSTALL_DATA) $< $@

