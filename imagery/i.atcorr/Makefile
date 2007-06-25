MODULE_TOPDIR = ../..

PGM = i.atcorr

include $(MODULE_TOPDIR)/include/Make/Module.make

SOURCES = AerosolConcentration.cpp Transform.cpp Iwave.cpp Interp.cpp GeomCond.cpp gauss.cpp computations.cpp AtmosModel.cpp Altitude.cpp AerosolModel.cpp Abstra.cpp main.cpp Output.cpp common.cpp 6s.cpp

HEADERS = 6s.h common.h Output.h Abstra.h AerosolModel.h Altitude.h AtmosModel.h Gauss.h GeomCond.h Interp.h Iwave.h Transform.h AerosolConcentration.h

OBJARCH=OBJ.$(ARCH)

$(OBJARCH)/%.o:%.cpp
	$(CXX) -c $(CXXFLAGS) $(NLS_CFLAGS) $< -o $@

WARNING_FLAGS   = -Wformat  -Wparentheses  -Wpointer-arith -Wno-conversion \
  -Wimplicit-int -Wimplicit-function-declaration -Wreturn-type	\
  -Wcomment  -Wno-sign-compare

LIBS = $(GISLIB) 
DEPLIBS = $(DEPGISLIB)

default: $(BIN)/$(PGM) htmlcmd

$(BIN)/$(PGM): $(SOURCES)
	$(CXX) $(LDFLAGS) $(ARCH_INC) -o $@ $(SOURCES) $(LIBS) $(MATHLIB)

