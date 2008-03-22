MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Lib.make

CXXFLAGS += $(SHLIB_CFLAGS) $(GDALCFLAGS) $(PYTHONCFLAGS) $(WXWIDGETSCXXFLAGS)

LDFLAGS += $(SHLIB_LDFLAGS) -L$(ARCH_LIBDIR) $(VECTLIB) $(GISLIB) $(GDALLIBS) $(VEDITLIB) $(WXWIDGETSLIB) $(PYTHONLDFLAGS) -lgdi

LOCAL_HEADERS = digit.h driver.h
SOURCES = driver.cpp digit.cpp cats.cpp line.cpp vertex.cpp select.cpp grass6_wxvdigit_wrap.cpp

OBJARCH = OBJ.$(ARCH)
OBJ := $(patsubst %.cpp, $(OBJARCH)/%.o, $(SOURCES))

ETCDIR = $(ETC)/wxpython

SHLIB = $(OBJARCH)/_grass6_wxvdigit$(SHLIB_SUFFIX)

EXTRA_CLEAN_FILES = $(SHLIB) grass6_wxvdigit.i grass6_wxvdigit.py grass6_wxvdigit_wrap.cpp

default: install_vdigit

grass6_wxvdigit.i: digit.i dig_types.i driver.h digit.h
	cat digit.i > grass6_wxvdigit.i
	cat dig_types.i >> grass6_wxvdigit.i
	echo "/* auto-generate swig typedef file */" >> grass6_wxvdigit.i
	cat driver.h >> grass6_wxvdigit.i
	cat digit.h >> grass6_wxvdigit.i

grass6_wxvdigit.py grass6_wxvdigit_wrap.cpp: grass6_wxvdigit.i
	$(SWIG) -c++ -python -shadow -o $@ $<

$(OBJARCH)/%.o: %.cpp $(LOCAL_HEADERS)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(SHLIB): $(OBJ)
	$(SHLIB_LD) $(LDFLAGS) $(OBJ) -o $(SHLIB) 

install_vdigit: $(SHLIB) grass6_wxvdigit.py
	$(INSTALL) $(SHLIB) $(ETCDIR)/vdigit/
	$(INSTALL_DATA) grass6_wxvdigit.py $(ETCDIR)/vdigit/
