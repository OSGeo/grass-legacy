SWIGBIN = /c/lang/SWIG-1.3.19
GRASSTOP = ../..
GRASSPLAT = dist.i686-pc-mingw32
TCLHOME = /e/lang/Tcl
TCLINC = $(TCLHOME)/include
TCLLIB = $(TCLHOME)/bin

all: libgis.dll

libgis.dll: libgis_wrap.c $(GRASSTOP)/$(GRASSPLAT)/lib/libgis.a
	gcc --share -I$(GRASSTOP)/src/include -I$(TCLINC) libgis_wrap.c -L$(GRASSTOP)/$(GRASSPLAT)/lib -lgis -ldatetime -L$(TCLLIB) -ltcl84 -loncrpc -lz -o libgis.dll >make.log 2>&1

libgis_wrap.c: libgis.i $(GRASSTOP)/src/include/gis.h $(GRASSTOP)/src/include/gisdefs.h
	$(SWIGBIN)/swig -I$(GRASSTOP)/src/include -tcl libgis.i