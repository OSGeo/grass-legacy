MODULE_TOPDIR =../..

PGM = r.li

#insert all subdirs AFTER r.li.daemon

SUBDIRS = \
	r.li.daemon\
	r.li.edgedensity\
	r.li.patchdensity\
	r.li.patchnum\
	r.li.setup \
	r.li.shape\
	r.li.simpson\
	r.li.shannon\
	r.li.mps\
	r.li.mpa\
	r.li.padcv\
	r.li.padsd\
	r.li.padrange\
	r.li.cwed\
	r.li.richness\
	r.li.dominance\

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: subdirs
	$(INSTALL_DATA) description.html $(GISBASE)/docs/html/r.li.html

clean: cleansubdirs
