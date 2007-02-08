MODULE_TOPDIR =../..

PGM = r.li

include $(MODULE_TOPDIR)/include/Make/Dir.make
include $(MODULE_TOPDIR)/include/Make/Html.make

#insert all subdirs AFTER r.li.daemon

SUBDIRS = \
	r.li.daemon\
	r.li.edgedensity\
	r.li.patchdensity\
	r.li.patchnumber\
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
	
default: subdirs htmldir

clean: cleansubdirs
