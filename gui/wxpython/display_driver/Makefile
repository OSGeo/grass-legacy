PYTHONVERSION=2.4

MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Lib.make
include $(MODULE_TOPDIR)/include/Make/Doxygen.make

SWIG=swig

CFLAGS=-c -fpic -I/usr/include/python$(PYTHONVERSION) -I./ -I$(ARCH_DISTDIR)/include `wx-config --cxxflags` `gdal-config --cflags`

LDFLAGS=-shared -L$(ARCH_LIBDIR) -lgrass_vect -lgrass_gis `wx-config --libs`

default: grass6_wxdriver.so

clean:
	-rm -f *.o *.so grass6_wxdriver_wrap.cxx grass6_wxdriver.py grass6_wxdriver.i grass6_wxdriver.pyc	 

grass6_wxdriver.i:
	cat ./driver.i > grass6_wxdriver.i
	echo "/* auto-generate swig typedef file (with some GRASS functions removed) */" >> grass6_wxdriver.i
	cat ./driver.h >> grass6_wxdriver.i

grass6_wxdriver_wrap.cxx: grass6_wxdriver.i
	$(SWIG) -c++ -python -shadow $<

grass6_wxdriver_wrap.o: grass6_wxdriver_wrap.cxx
	$(CXX) $(CFLAGS) $(INCLUDE_DIRS) $<

driver.o: driver.cc
	$(CXX) $(CFLAGS) $(INCLUDE_DIRS) $<

pseudodc.o: pseudodc.cpp
	$(CXX) $(CFLAGS) $(INCLUDE_DIRS) $<

grass6_wxdriver.so: grass6_wxdriver_wrap.o driver.o pseudodc.o
	$(CXX) $(LDFLAGS) grass6_wxdriver_wrap.o driver.o pseudodc.o -o _grass6_wxdriver.so
