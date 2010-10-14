MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Dir.make
include $(MODULE_TOPDIR)/include/Make/Python.make

SCRIPTDIR = $(ETC)/wxpython/scripts

ifdef MINGW
PYFILES := $(patsubst %.py, $(SCRIPTDIR)/%.py, $(filter %.py, $(PY_SOURCES)))
else
PYFILES := $(patsubst %.py, $(SCRIPTDIR)/%, $(filter %.py, $(PY_SOURCES)))
endif

default: install_scripts

install_scripts:
	$(MAKE) $(PYFILES)

$(SCRIPTDIR)/%: %.py | $(SCRIPTDIR)
	$(INSTALL) $< $@

$(SCRIPTDIR)/%.py: %.py | $(SCRIPTDIR)
	$(INSTALL) $< $@

$(SCRIPTDIR):
	-test -d $@ | $(MKDIR) $@
