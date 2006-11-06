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
	r.li.meanPatchSize\
	r.li.meanPixelAttribute\
	r.li.patchAreaDistributionCV\
	r.li.contrastWeightedEdgeDensity\


default: subdirs htmldir

clean: cleansubdirs
