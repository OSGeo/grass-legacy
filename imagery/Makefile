
MODULE_TOPDIR = ..


SUBDIRS1 = \
	i.ask \
	i.class \
	i.cluster \
	i.find \
	i.gensig \
	i.gensigset \
	i.group \
	i.his.rgb \
	i.maxlik \
	i.ortho.photo \
	i.points \
	i.rectify \
	i.rgb.his \
	i.smap \
	i.target

FFTWBASED = i.cca i.fft i.ifft i.pca i.zc

include $(MODULE_TOPDIR)/include/Make/Dir.make

#compile if FFTW present:
ifneq ($(strip $(FFTWLIB)),)
    SUBDIRS2 = $(SUBDIRS1) $(FFTWBASED)
else
    SUBDIRS2 = $(SUBDIRS1)
endif

SUBDIRS = $(SUBDIRS2)
default: subdirs

clean: cleansubdirs
