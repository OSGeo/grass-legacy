MODULE_TOPDIR = ../..

SUBDIRS = docs vdigit nviz
EXTRA_CLEAN_FILES = menustrings.py
CLEAN_SUBDIRS = vdigit nviz

include $(MODULE_TOPDIR)/include/Make/Dir.make
include $(MODULE_TOPDIR)/include/Make/Doxygen.make
include $(MODULE_TOPDIR)/include/Make/Python.make

ETCDIR = $(ETC)/wxpython

SRCFILES := $(wildcard scripts/* compat/* gui_modules/* icons/*.* icons/silk/* images/* xml/*) gis_set.py wxgui.py README scripts/wxgui
DSTFILES := $(patsubst %,$(ETCDIR)/%,$(SRCFILES)) $(patsubst %.py,$(ETCDIR)/%.pyc,$(filter %.py,$(SRCFILES)))

default: install_scripts
	$(MAKE) parsubdirs

install_scripts:
	-for dir in '' compat gui_modules icons icons/silk images scripts vdigit xml nviz ; do \
	if [ ! -d $(ETCDIR)/$$dir ] ; then $(MKDIR) $(ETCDIR)/$$dir ; fi ; \
	done
	$(MAKE) $(DSTFILES)
	$(MAKE) menustrings.py

$(ETCDIR)/scripts/wxgui: wxgui
	$(INSTALL) $< $@

$(ETCDIR)/scripts/%: scripts/%
	$(INSTALL) $< $@

$(ETCDIR)/%: %
	$(INSTALL_DATA) $< $@

menustrings.py: gui_modules/menudata.py $(ETCDIR)/xml/menudata.xml $(ETCDIR)/xml/menudata_modeler.xml 
	GISBASE="$(GISBASE)" \
	$(PYTHON) $< > $@
	GISBASE="$(GISBASE)" \
	$(PYTHON) $< "modeler" >> $@

#doxygen:
DOXNAME=wxpython
