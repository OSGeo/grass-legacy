MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Dir.make

SUBDIRS = \
	r.average \
	r.basins.fill \
	r.bilinear \
	r.buffer \
	r.cats \
	r.circle \
	r.clump \
	r.coin \
	r.colors \
	r.composite \
	r.compress \
	r.covar \
	r.cross \
	r.describe \
	r.digit \
	r.distance \
	r.drain \
	r.fill.dir \
	r.grow \
	r.his \
	r.in.arc \
	r.in.ascii \
	r.in.bin \
	r.info \
	r.in.gridatb \
	r.in.poly \
	r.kappa \
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
	r.random.cells \
	r.random.surface \
	r.reclass \
	r.recode \
	r.region \
	r.report \
	r.resample \
	r.rescale \
	r.rescale.eq \
	r.series \
	r.slope.aspect \
	r.statistics \
	r.stats \
	r.sum \
	r.sun \
	r.sunmask \
	r.surf.area \
	r.surf.contour \
	r.surf.idw \
	r.texture \
	r.thin \
	r.timestamp \
	r.topidx \
	r.topmodel \
	r.transect \
	r.water.outlet \
	r.what \
	wildfire

GDALBASED = r.in.gdal

FFTWBASED = r.surf.fractal r.surf.gauss r.surf.random

PNGBASED = r.out.png

CXXBASED = r.terraflow

#compile if GDAL present:
ifneq ($(strip $(GDALLIBS)),)
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
