
MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

# Note that the file list in the tar command should include everything but
# the Gmakefile
#

default:
	if [ ! -d $(GISBASE)/bwidget ]; then mkdir $(GISBASE)/bwidget; fi
	tar cf - images/* lang/* *.txt *.tcl README.grass | (cd $(GISBASE)/bwidget; tar xf -)

clean:
