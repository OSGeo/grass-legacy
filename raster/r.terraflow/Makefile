MODULE_TOPDIR = ../..

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

OBJ := $(patsubst %.cc,%.o,$(SOURCES))
FLOAT_OBJ = $(patsubst %.cc, $(OBJARCH)/FLOAT/%.o, $(SOURCES))
SHORT_OBJ = $(patsubst %.cc, $(OBJARCH)/SHORT/%.o, $(SOURCES))


#Note: 	if a header file is modified, the .o files do not get rebuilt..
#		header files should be included as prerequisites, but does not work 
#		because of GRASS scripts 
$(OBJARCH)/FLOAT/%.o:%.cc
	$(CXX) -c $(CXXFLAGS) $(GRASS_INC) -DELEV_FLOAT $< -o $@

$(OBJARCH)/SHORT/%.o:%.cc
	$(CXX) -c $(CXXFLAGS) $(GRASS_INC) -DELEV_SHORT $< -o $@


IOSTREAM_DIR =  ./IOStream
IOSTREAM_INC = $(IOSTREAM_DIR)/include
IOSTREAM_LIBDIR = $(IOSTREAM_DIR)/lib
IOSTREAM_LIB =  -liostream


WARNING_FLAGS   = -Wformat  -Wparentheses  -Wpointer-arith -Wno-conversion \
  -Wimplicit-int -Wimplicit-function-declaration -Wreturn-type	\
  -Wcomment  -Wno-sign-compare


CXXFLAGS += -I$(IOSTREAM_INC) \
		-DUSER=\"$(USER)\" \
		-DNODATA_FIX -D_FILE_OFFSET_BITS=64

LDFLAGS += -L$(IOSTREAM_LIBDIR)

LIBS = $(GISLIB) 
DEPLIBS = $(DEPGISLIB)

PGM = r.terraflow

default: dofirst $(BIN_CMD)/$(PGM) $(BIN_CMD)/$(PGM).short htmlcmd

dofirst:
	-mkdir -p $(OBJARCH)/FLOAT ; true
	-mkdir -p $(OBJARCH)/SHORT ; true

$(BIN_CMD)/$(PGM): $(FLOAT_OBJ) $(GISLIB)  $(DEPLIBS)
	(cd IOStream/lib ; make)
	$(CXX) -DELEV_FLOAT $(LDFLAGS) -o $@ $(FLOAT_OBJ) $(LIBS) $(MATHLIB) \
	$(XDRLIB) $(IOSTREAM_LIB)
	@test -x $(BIN)/$(PGM) || ln $(ETC)/front.end $(BIN)/$(PGM)

$(BIN_CMD)/$(PGM).short: $(SHORT_OBJ) $(GISLIB)  $(DEPLIBS)
	(cd IOStream/lib ; make)
	$(CXX) -DELEV_SHORT $(LDFLAGS) -o $@ $(SHORT_OBJ) $(LIBS) $(MATHLIB) \
	$(XDRLIB) $(IOSTREAM_LIB)
	@test -x $(BIN)/$(PGM).short || ln $(ETC)/front.end $(BIN)/$(PGM).short


$(GISLIB):	#
$(DEPGISLIB):	#

htmlcmd:
	@test ! -f description.html || ( cat description.html >> $(PGM).html )
	echo "<HR>" >> $(PGM).html
	echo "<P><a href=index.html>Help Index</a>" >> $(PGM).html
	echo "</body></html>" >> $(PGM).html
	mkdir -p $(GISBASE)/docs/html
	mv $(PGM).html $(GISBASE)/docs/html
	cp *.png $(GISBASE)/docs/html

clean: 
	(cd IOStream/lib ; make clean )
	-rm $(OBJARCH)/FLOAT/*
	-rm $(OBJARCH)/SHORT/*
	-rmdir $(OBJARCH)/FLOAT
	-rmdir $(OBJARCH)/SHORT
	-rmdir $(OBJARCH)
