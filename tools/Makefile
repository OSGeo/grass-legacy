MODULE_TOPDIR = ..

PGM1=timer
PGM2=getModTime
PGM3=grass-run.sh

PROGDIR =  $(GISBASE)/scripts/

include $(MODULE_TOPDIR)/include/Make/Module.make

PGM1:
	$(MAKE) -C timer

PGM2:
	$(MAKE) -C getModTime

PGM3:
	$(MAKE) -C grass-run.sh

default: PGM1 PGM2 PGM3

clean:
	-rm -rf $(OBJDIR)
	$(MAKE) -C timer clean
	$(MAKE) -C getModTime clean
	$(MAKE) -C grass-run.sh clean
