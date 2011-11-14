MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make
include $(MODULE_TOPDIR)/include/Make/Python.make
include $(MODULE_TOPDIR)/include/Make/Doxygen.make

PYDIR = $(ETC)/python
GDIR = $(PYDIR)/grass
DSTDIR = $(GDIR)/script

MODULES = core db raster vector array setup task

PYFILES := $(patsubst %,$(DSTDIR)/%.py,$(MODULES) __init__)
PYCFILES := $(patsubst %,$(DSTDIR)/%.pyc,$(MODULES) __init__)

CLEAN_SUBDIRS = ctypes
EXTRA_CLEAN_FILES = setup.py

default: $(PYFILES) $(PYCFILES) $(GDIR)/__init__.py $(GDIR)/__init__.pyc
	-$(MAKE) -C ctypes || echo $(CURDIR)/ctypes >> $(ERRORLOG)

$(PYDIR):
	$(MKDIR) $@

$(GDIR):
	$(MAKE) $(PYDIR)
	$(MKDIR) $@

$(DSTDIR):
	$(MAKE) $(GDIR)
	$(MKDIR) $@

$(GDIR)/__init__.py: grass__init__.py
	$(MAKE) $(GDIR)
	$(INSTALL_DATA) $< $@

$(DSTDIR)/%: %
	$(MAKE) $(DSTDIR)
	$(INSTALL_DATA) $< $@

setup.py: setup.py.sed
	sed \
	-e 's#@LD_LIBRARY_PATH_VAR@#$(LD_LIBRARY_PATH_VAR)#' \
	$< > $@

#doxygen:
DOXNAME = python
