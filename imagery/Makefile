MODULE_TOPDIR = ..

SUBDIRS = \
	i.ask \
	i.cluster \
	i.find \
	i.gensig \
	i.gensigset \
	i.group \
	i.his.rgb \
	i.landsat.acca \
	i.landsat.toar \
	i.maxlik \
	i.modis.qc \
	i.rectify \
	i.rgb.his \
	i.smap \
	i.target \
	i.topo.corr \
	i.pca \
	i.cca

include $(MODULE_TOPDIR)/include/Make/Platform.make

CXXBASED = i.atcorr

FFTWBASED = i.fft i.ifft i.zc

XMONBASED = i.class i.ortho.photo i.points i.vpoints

#compile if C++ compiler present:
ifneq ($(strip $(CXX)),)
    SUBDIRS += $(CXXBASED)
endif

#compile if FFTW present:
ifneq ($(strip $(FFTWLIB)),)
    SUBDIRS += $(FFTWBASED)
endif

#compile if interactive Xmons are present:
ifneq ($(USE_X11),)
    SUBDIRS += $(XMONBASED)
endif

PGM = imageryintro

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: htmldir

htmldir: parsubdirs

clean: cleansubdirs
