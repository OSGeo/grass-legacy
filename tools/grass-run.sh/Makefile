MODULE_TOPDIR = ../..

PGM = grass-run.sh

include $(MODULE_TOPDIR)/include/Make/Script.make

default:
	$(INSTALL) -m 755 $(PGM) $(GISBASE)/etc 2> /dev/null ; true

clean:
	\rm -f $(ETC)/$(PGM)
