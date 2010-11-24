MODULE_TOPDIR =../..

PGM = r.li

SUBDIRS1 = \
	r.li.cwed \
	r.li.dominance \
	r.li.edgedensity \
	r.li.mpa \
	r.li.mps \
	r.li.padcv \
	r.li.padrange \
	r.li.padsd \
	r.li.patchdensity \
	r.li.patchnum \
	r.li.richness \
	r.li.pielou \
	r.li.renyi \
	r.li.setup \
	r.li.shannon \
	r.li.shape \
	r.li.simpson

SUBDIRS = r.li.daemon $(SUBDIRS1)

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs
	$(INSTALL_DATA) description.html $(GISBASE)/docs/html/r.li.html

clean: cleansubdirs

$(SUBDIRS1): r.li.daemon

