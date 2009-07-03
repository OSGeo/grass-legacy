MODULE_TOPDIR = ..

#compile if PYTHON present:
ifneq ($(USE_PYTHON),)
ifneq ($(strip $(CXX)),)
    SUBDIRS += python
endif
endif

# doxygen:
DOXNAME=swig

include $(MODULE_TOPDIR)/include/Make/Dir.make
include $(MODULE_TOPDIR)/include/Make/Doxygen.make

default:
	@echo "Please run 'make' in the individual subdirectories"
