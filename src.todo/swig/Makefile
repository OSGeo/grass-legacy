# Where swig executable lives
SWIGBIN = /c/lang/SWIG-1.3.19

# Top of Grass hierarchy
GRASSTOP = ../..
GRASSPLAT = dist.i686-pc-mingw32

# Where TCL lives
TCLHOME = /e/lang/Tcl
#TCLHOME = /c/lang/Tcl-8.4.1.0

# TCL lib and include dirs
TCLINC = $(TCLHOME)/include
TCLLIB = $(TCLHOME)/bin

# File names
DLLEXT = dll
BINDNAME = libgis

all: $(BINDNAME).$(DLLEXT)

$(BINDNAME).$(DLLEXT): $(BINDNAME)_wrap.c $(GRASSTOP)/$(GRASSPLAT)/lib/$(BINDNAME).a
	gcc -shared  $(BINDNAME)_wrap.c -I$(GRASSTOP)/src/include -I$(TCLINC) \
	   -L$(GRASSTOP)/$(GRASSPLAT)/lib -lgis -ldatetime \
	   -L$(TCLLIB) -ltcl84 -lrpclib -lz -o $(BINDNAME).$(DLLEXT)

$(BINDNAME)_wrap.c: $(BINDNAME).i $(GRASSTOP)/src/include/gis.h $(GRASSTOP)/src/include/gisdefs.h
	$(SWIGBIN)/swig -I$(GRASSTOP)/src/include -tcl $(BINDNAME).i

binary-clean:
	rm -f *.a *.o $(BINDNAME)_wrap.c $(BINDNAME).$(DLLEXT)

clean : binary-clean
	rm -f *.log *~