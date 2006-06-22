MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

# Note that the file list in the tar command should include everything but
# the Gmakefile
#

default:
	if [ ! -d $(GISBASE)/bwidget ]; then $(MKDIR) $(GISBASE)/bwidget; fi
	tar chf - images/* lang/* *.txt README.grass | (cd $(GISBASE)/bwidget; tar xf -)
	$(INSTALL) -m 755 *.tcl $(GISBASE)/bwidget/

clean:
