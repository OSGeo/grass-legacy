MODULE_TOPDIR = ../..

PGM = r.terraflow

include $(MODULE_TOPDIR)/include/Make/Module.make

SOURCES = main.cc common.cc stats.cc fill.cc types.cc ccforest.cc \
	nodata.cc plateau.cc direction.cc water.cc  \
	filldepr.cc grid.cc genericWindow.cc \
	flow.cc sweep.cc weightWindow.cc

HEADERS = common.h stats.h streamutils.h types.h \
	option.h fill.h main.h \
	nodata.h ccforest.h	sortutils.h 3scan.h\
	plateau.h direction.h genericWindow.h \
	water.h grass2str.h filldepr.h unionFind.h \
	grid.h genericWindow.h flow.h sweep.h \
	weightWindow.h

OBJARCH=OBJ.$(ARCH)

FLOAT_OBJ := $(patsubst %.cc, $(OBJARCH)/FLOAT/%.o, $(SOURCES))
SHORT_OBJ := $(patsubst %.cc, $(OBJARCH)/SHORT/%.o, $(SOURCES))

IOSTREAM_DIR =  ./IOStream
IOSTREAM_INC = $(IOSTREAM_DIR)/include
IOSTREAM_LIBDIR = $(IOSTREAM_DIR)/lib
IOSTREAM_LIB =  -liostream
IOSTREAM_DEPLIB = $(IOSTREAM_LIBDIR)/libiostream.a

WARNING_FLAGS   = -Wformat  -Wparentheses  -Wpointer-arith -Wno-conversion \
  -Wimplicit-int -Wimplicit-function-declaration -Wreturn-type	\
  -Wcomment  -Wno-sign-compare


CXXFLAGS += -I$(IOSTREAM_INC) \
		-DUSER=\"$(USER)\" \
		-DNODATA_FIX -D_FILE_OFFSET_BITS=64

LDFLAGS += -L$(IOSTREAM_LIBDIR)

LIBS = $(GISLIB) 
DEPLIBS = $(DEPGISLIB)

CLEAN_SUBDIRS = $(IOSTREAM_LIBDIR)

#Note: 	if a header file is modified, the .o files do not get rebuilt..
#		header files should be included as prerequisites, but does not work 
#		because of GRASS scripts 
$(OBJARCH)/FLOAT/%.o: %.cc | $(OBJARCH)/FLOAT
	$(CXX) -c $(CXXFLAGS) $(NLS_CFLAGS) -DELEV_FLOAT $< -o $@

$(OBJARCH)/SHORT/%.o: %.cc | $(OBJARCH)/SHORT
	$(CXX) -c $(CXXFLAGS) $(NLS_CFLAGS) -DELEV_SHORT $< -o $@

default: $(BIN)/$(PGM) $(BIN)/$(PGM).short htmlcmd

$(OBJARCH)/FLOAT:
	$(MKDIR) $@

$(OBJARCH)/SHORT:
	$(MKDIR) $@

$(BIN)/$(PGM): $(FLOAT_OBJ) $(DEPLIBS) $(IOSTREAM_DEPLIB)
	$(CXX) -DELEV_FLOAT $(LDFLAGS) -o $@ $(FLOAT_OBJ) $(LIBS) $(MATHLIB) \
	$(XDRLIB) $(IOSTREAM_LIB)

$(BIN)/$(PGM).short: $(SHORT_OBJ) $(DEPLIBS) $(IOSTREAM_DEPLIB)
	$(CXX) -DELEV_SHORT $(LDFLAGS) -o $@ $(SHORT_OBJ) $(LIBS) $(MATHLIB) \
	$(XDRLIB) $(IOSTREAM_LIB)

$(IOSTREAM_DEPLIB):
	$(MAKE) -C $(IOSTREAM_LIBDIR)

