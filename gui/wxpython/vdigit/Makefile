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

SHLIB = _grass6_wxvdigit$(SHLIB_SUFFIX)

default: install_vdigit

clean:
	-rm -rf $(OBJARCH) _grass6_wxvdigit.so grass6_wxvdigit.i	 

grass6_wxvdigit.i:
	cat ./digit.i > grass6_wxvdigit.i
	cat ./dig_types.i >> grass6_wxvdigit.i
	echo "/* auto-generate swig typedef file */" >> grass6_wxvdigit.i
	cat ./driver.h >> grass6_wxvdigit.i
	cat ./digit.h >> grass6_wxvdigit.i

create_wrap.cpp: grass6_wxvdigit.i
	$(SWIG) -c++ -python -shadow -o grass6_wxvdigit_wrap.cpp $<

$(OBJARCH)/%.o: %.cpp $(LOCAL_HEADERS)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(SHLIB): $(OBJ)
	$(SHLIB_LD) $(LDFLAGS) $(OBJ) -o $(SHLIB) 

install_vdigit: $(SHLIB)
	$(INSTALL_DATA) $(SHLIB) $(ETCDIR)/vdigit/
	$(INSTALL_DATA) grass6_wxvdigit.py $(ETCDIR)/vdigit/
