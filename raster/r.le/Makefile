
MODULE_TOPDIR = ../..

SUBDIRS = \
	r.le.patch \
	r.le.pixel
# r.le.setup/polytocell

XMONBASED = r.le.setup r.le.trace


include $(MODULE_TOPDIR)/include/Make/Platform.make

#compile if interactive Xmons are present:
ifneq ($(USE_X11),)
    SUBDIRS += $(XMONBASED)
endif


include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs

clean: cleansubdirs
