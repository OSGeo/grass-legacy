MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Dir.make

SUBDIRS = \
	r.average \
	r.basins.fill \
	r.bilinear \
	r.bitpattern \
	r.buffer \
	r.cats \
	r.circle \
	r.clump \
	r.coin \
	r.colors \
	r.composite \
	r.compress \
	r.contour \
	r.cost \
	r.covar \
	r.cross \
	r.describe \
	r.digit \
	r.distance \
	r.drain \
	r.fill.dir \
	r.flow \
	r.grow2 \
	r.his \
	r.in.arc \
	r.in.ascii \
	r.in.bin \
	r.info \
	r.in.gridatb \
	r.in.mat \
	r.in.poly \
	r.kappa \
	r.le \
	r.los \
	r.mapcalc \
	r.median \
	r.mfilter \
	r.mode \
	r.neighbors \
	r.null \
	r.out.arc \
	r.out.ascii \
	r.out.bin \
	r.out.gridatb \
	r.out.mat \
	r.out.mpeg \
	r.out.pov \
	r.out.ppm \
	r.out.ppm3 \
	r.out.tiff \
	r.param.scale \
	r.patch \
	r.profile \
	r.proj \
	r.quant \
	r.random \
	r.random.cells \
	r.random.surface \
	r.reclass \
	r.recode \
	r.region \
	r.report \
	r.resample \
	r.resamp.rst \
	r.rescale \
	r.rescale.eq \
	r.series \
	r.slope.aspect \
	r.statistics \
	r.stats \
	r.sum \
	r.sun \
	r.sunmask \
	r.support \
	r.surf.area \
	r.surf.contour \
	r.surf.idw \
	r.texture \
	r.thin \
	r.timestamp \
	r.to.vect \
	r.topidx \
	r.topmodel \
	r.transect \
	r.univar2 \
	r.water.outlet \
	r.watershed \
	r.what \
	wildfire

GDALBASED = r.in.gdal

FFTWBASED = r.surf.fractal r.surf.gauss r.surf.random

PNGBASED = r.out.png

CXXBASED = r.terraflow

#compile if GDAL present:
ifneq ($(USE_GDAL),)
    SUBDIRS += $(GDALBASED)
endif

#compile if FFTW present:
ifneq ($(strip $(FFTWLIB)),)
    SUBDIRS += $(FFTWBASED)
endif

#compile if PNG present:
ifneq ($(strip $(PNGLIB)),)
    SUBDIRS += $(PNGBASED)
endif

#compile if C++ compiler present:
ifneq ($(strip $(CXX)),)
    SUBDIRS += $(CXXBASED)
endif

default: subdirs

clean: cleansubdirs
