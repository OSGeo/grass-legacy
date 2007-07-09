MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Dir.make

SUBDIRS = \
	r3.cross.rast \
	r3.in.ascii \
	r3.in.v5d \
	r3.gwflow \
	r3.out.ascii \
	r3.out.v5d \
	r3.out.vtk \
	r3.stats \
	r3.to.rast \
	base

OPENGLBASED = r3.mkdspf

#compile if OPENGLU present:
ifneq ($(strip $(OPENGLULIB)),)
    SUBDIRS += $(OPENGLBASED)
endif

default: htmldir

htmldir: subdirs

clean: cleansubdirs

PGM = raster3dintro
