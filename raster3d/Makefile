MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Dir.make

SUBDIRS = \
	r3.in.ascii \
	r3.info \
	r3.in.v5d \
	r3.mask \
	r3.null \
	r3.out.ascii \
	r3.out.v5d \
	r3.timestamp

OPENGLBASED = r3.mkdspf

#compile if OPENGLU present:
ifneq ($(strip $(OPENGLULIB)),)
    SUBDIRS += $(OPENGLBASED)
endif

default: subdirs

clean: cleansubdirs
