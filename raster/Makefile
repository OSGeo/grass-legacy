MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Dir.make

SUBDIRS1 = \
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
	r.cost \
	r.covar \
	r.cross \
	r.describe \
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
	r.out.mpeg \
	r.out.png \
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
	r.report \
	r.resample \
	r.rescale \
	r.rescale.eq \
	r.slope.aspect \
	r.statistics \
	r.stats \
	r.sum \
	r.sun \
	r.sunmask \
	r.surf.area \
	r.surf.contour \
	r.surf.gauss \
	r.surf.idw \
	r.surf.idw2 \
	r.surf.random \
	r.thin \
	r.timestamp \
	r.topidx \
	r.topmodel \
	r.transect \
	r.water.outlet \
	r.what \
	wildfire

GDALBASED = r.in.gdal

FFTWBASED = r.surf.fractal

#compile if GDAL present:
ifneq ($(strip $(GDALLIBS)),)
    SUBDIRS2 = $(GDALBASED)
endif

#compile if FFTW present:
ifneq ($(strip $(FFTWLIB)),)
    SUBDIRS3 = $(FFTWBASED)
endif

SUBDIRS = $(SUBDIRS1) $(SUBDIRS2) $(SUBDIRS3)

default: subdirs

clean: cleansubdirs
