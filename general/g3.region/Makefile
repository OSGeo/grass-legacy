
MODULE_TOPDIR = ../..

PGM = g3.region

LIBES     = $(G3DLIB) $(VECTLIB) $(GPROJLIB) $(GISLIB) $(DATETIMELIB) $(VASKLIB) $(MATHLIB) $(XDRLIB)
DEPENDENCIES = $(G3DDEP) $(GISDEP) $(VECTDEP) $(DATETIMEDEP) $(VASKDEP) $(GPROJDEP)
EXTRA_INC = $(VECT_INC) $(PROJINC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd
	
htmlcmd1:
	$(MKDIR) $(GISBASE)/docs/html
	$(INSTALL) -m 644 description.html $(GISBASE)/docs/html/$(PGM).html
