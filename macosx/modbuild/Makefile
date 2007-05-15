# Makefile for building Mac OS X addon module build skeleton

MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

ifndef MACOSX_APP_NAME
	MACOSX_APP_NAME = GRASS-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.app
endif
MACOSX_APPDIR = ${MACOSX_APP_NAME}/Contents/Resources
INST_DIR_MACOSX = ${prefix}/${MACOSX_APPDIR}

default: modbuild

modbuild: ReadMe.rtf
	mkdir -p $(ARCH_DISTDIR)/modbuild
	-cp -f License.rtf $(ARCH_DISTDIR)/modbuild/
	-cp -f ReadMe.rtf $(ARCH_DISTDIR)/modbuild/
	mkdir -p $(ARCH_DISTDIR)/modbuild/dist.$(ARCH)
	-cp -rf $(ARCH_DISTDIR)/demolocation $(ARCH_DISTDIR)/modbuild/dist.$(ARCH)/
	mkdir -p $(ARCH_DISTDIR)/modbuild/module
	mkdir -p $(ARCH_DISTDIR)/modbuild/include/Make
	-cp $(ARCH_DISTDIR)/include/Make/Dir.make $(ARCH_DISTDIR)/modbuild/include/Make/
	-cp $(ARCH_DISTDIR)/include/Make/Doxygen.make $(ARCH_DISTDIR)/modbuild/include/Make/
	@# need a little change in Grass.make
	-sed -e 's,@GRASS_LIB_PREFIX@,lib,g' \
	-e 's,@GRASS_LIB_SUFFIX@,.dylib,g' \
	-e 's,@GRASS_LIBRARY_TYPE@,shlib,g' \
	-e 's,@GRASS_VERSION_MAJOR@,$(GRASS_VERSION_MAJOR),g' \
	-e 's,@GRASS_VERSION_MINOR@,$(GRASS_VERSION_MINOR),g' \
	-e 's,@GRASS_VERSION_RELEASE@,$(GRASS_VERSION_RELEASE),g' \
	-e 's,@GRASS_VERSION_DATE@,$(GRASS_VERSION_DATE),g' \
	-e 's#^GISBASE.*#GISBASE = $$(GRASS_APP)/Contents/Resources#g' \
	../../include/Make/Grass.make.in > $(ARCH_DISTDIR)/modbuild/include/Make/Grass.make
	@# until I figure out how to get sed to replace with multiple lines in a
	@# makefile, GRASS_APP required for modbuild, ie no default to /Applications
	@#-e 's#^GISBASE.*#ifdef GRASS_APP\
	@#GISBASE = $$(GRASS_APP)/Contents/Resources\
	@#else\
	@#GISBASE = $(INST_DIR_MACOSX)\
	@#endif#g'
	@# html.make needs a little change
	sed -e 's,GISRC=$$(RUN_GISBASE),GISRC=$$(ARCH_DISTDIR),g' ../../include/Make/Html.make > $(ARCH_DISTDIR)/modbuild/include/Make/Html.make
	-cp $(ARCH_DISTDIR)/include/Make/Lib.make $(ARCH_DISTDIR)/modbuild/include/Make/
	sed -e 's,$$(DEPENDENCIES),,g' $(ARCH_DISTDIR)/include/Make/Module.make > $(ARCH_DISTDIR)/modbuild/include/Make/Module.make
	@# platform.make changes
	sed -e 's,^GRASS_HOME.*,GRASS_HOME = $$(GRASS_HOME),g' \
	-e 's,^RUN_GISBASE.*,RUN_GISBASE = $$(GISBASE),g' \
	$(ARCH_DISTDIR)/include/Make/Platform.make > $(ARCH_DISTDIR)/modbuild/include/Make/Platform.make
	-cp $(ARCH_DISTDIR)/include/Make/Rules.make $(ARCH_DISTDIR)/modbuild/include/Make/
	-cp $(ARCH_DISTDIR)/include/Make/Script.make $(ARCH_DISTDIR)/modbuild/include/Make/
	-cp $(ARCH_DISTDIR)/include/Make/Shlib.make $(ARCH_DISTDIR)/modbuild/include/Make/
	-cp $(ARCH_DISTDIR)/include/Make/Stlib.make $(ARCH_DISTDIR)/modbuild/include/Make/
	mkdir -p $(ARCH_DISTDIR)/modbuild/tools/g.html2man
	-cp ../../tools/g.html2man/g.html2man $(ARCH_DISTDIR)/modbuild/tools/g.html2man/
	-cp ../../tools/mkhtml.sh $(ARCH_DISTDIR)/modbuild/tools/

clean: 
	
