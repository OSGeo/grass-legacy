
# common dependencies and rules for building subdirs

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

subdirs:
	@list='$(SUBDIRS)'; \
	for subdir in $$list; do \
	    echo $$subdir ; \
	    $(MAKE) -C $$subdir; \
	done

cleansubdirs:
	@list='$(SUBDIRS)'; \
	for subdir in $$list; do \
	    echo $$subdir ; \
	    $(MAKE) -C $$subdir clean; \
	done

