# Makefile for building Mac OS X app startup

MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

EXTRA_INC = 
EXTRA_CFLAGS = 
XTRA_LDFLAGS = -framework Cocoa -framework AppleScriptKit
ifndef MACOSX_APP_NAME
MACOSX_APP_NAME = GRASS-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.app
endif
APPDIR = $(ARCH_DISTDIR)/${MACOSX_APP_NAME}/Contents
TCLTK_INTERNAL = \#

OBJS = Info.plist GRASS.scpt grass.sh main.o

ARCH_OBJS := $(foreach obj,$(OBJS),$(OBJDIR)/$(obj)) 

# ugly hack - assume tcl and tk in same prefix, and same as TCLTKLIBPATH minus 'lib'
ifndef TCLTKPREFIX
TCLTKPREFIX = `echo ${TCLTKLIBPATH} | sed 's,-L,,' | sed 's,/lib *$$,,'`
endif
# ugly hack - assume gdal prefix starts with -L flag
ifndef GDAL_BIN
GDAL_BIN = `echo ${GDALLIBS} | sed 's,-L,,' | sed 's,/lib.*$$,/bin,'`
endif

default: macosxapp

macosxapp: PkgInfo app.icns English.lproj/MainMenu.nib build_html_user_index.sh $(ARCH_OBJS)
	@test -d $(APPDIR)/MacOS || mkdir -p $(APPDIR)/MacOS
	$(CC) $(LDFLAGS) -o $(APPDIR)/MacOS/GRASS $(OBJDIR)/main.o $(XTRA_LDFLAGS)
	@test -d $(APPDIR)/Resources/Scripts || mkdir -p $(APPDIR)/Resources/Scripts
	@test -d $(APPDIR)/Resources/etc || mkdir -p $(APPDIR)/Resources/etc
	-cp -f app.icns $(APPDIR)/Resources/
	-cp -rf English.lproj $(APPDIR)/Resources/
	-cp -f $(OBJDIR)/Info.plist $(APPDIR)/
	-cp -f PkgInfo $(APPDIR)/
	-cp -f $(OBJDIR)/grass.sh $(APPDIR)/Resources/
	-cp -f $(OBJDIR)/GRASS.scpt $(APPDIR)/Resources/Scripts/
	-cp -f build_html_user_index.sh $(APPDIR)/Resources/etc/
	-cp -f grass_icon.png $(APPDIR)/Resources/docs/html/
	@# TclTk embedding:
ifdef OPENGL_X11
	@test -d $(APPDIR)/Resources/bin || mkdir -p $(APPDIR)/Resources/bin
	-cp -f ${TCLTKPREFIX}/bin/tclsh8.4 $(APPDIR)/Resources/bin/
	-cp -f ${TCLTKPREFIX}/bin/wish8.4 $(APPDIR)/Resources/bin/
	@test -d $(APPDIR)/Resources/lib || mkdir -p $(APPDIR)/Resources/lib
	-cp -f ${TCLTKPREFIX}/lib/libtcl8.4.dylib $(APPDIR)/Resources/lib/
	-cp -f ${TCLTKPREFIX}/lib/libtk8.4.dylib $(APPDIR)/Resources/lib/
	-cp -rf ${TCLTKPREFIX}/lib/tcl8.4 $(APPDIR)/Resources/lib/
	-cp -rf ${TCLTKPREFIX}/lib/tk8.4 $(APPDIR)/Resources/lib/
	@test -d $(APPDIR)/Resources/include || mkdir -p $(APPDIR)/Resources/include
	-cp -f ${TCLTKPREFIX}/include/tcl.h $(APPDIR)/Resources/include/
	-cp -f ${TCLTKPREFIX}/include/tclDecls.h $(APPDIR)/Resources/include/
	-cp -f ${TCLTKPREFIX}/include/tclPlatDecls.h $(APPDIR)/Resources/include/
	-cp -f ${TCLTKPREFIX}/include/tk.h $(APPDIR)/Resources/include/
	-cp -f ${TCLTKPREFIX}/include/tkDecls.h $(APPDIR)/Resources/include/
	-cp -f ${TCLTKPREFIX}/include/tkPlatDecls.h $(APPDIR)/Resources/include/
endif

$(OBJDIR)/main.o: main.m
	@test -d $(OBJDIR) || mkdir $(OBJDIR)
	$(CC) $(CFLAGS) $(EXTRA_CFLAGS) $(EXTRA_INC) -o $(OBJDIR)/main.o -c main.m

$(OBJDIR)/Info.plist: Info.plist.in
	@test -d $(OBJDIR) || mkdir $(OBJDIR)
	sed -e "s,@GRASS_VERSION_MAJOR@,$(GRASS_VERSION_MAJOR),g" \
	-e "s,@GRASS_VERSION_MINOR@,$(GRASS_VERSION_MINOR),g" \
	-e "s,@GRASS_VERSION_RELEASE@,$(GRASS_VERSION_RELEASE),g" \
	Info.plist.in > $(OBJDIR)/Info.plist

$(OBJDIR)/GRASS.scpt: GRASS.applescript
	@test -d $(OBJDIR) || mkdir $(OBJDIR)
	osacompile -d -x -i /System/Library/Frameworks/AppleScriptKit.framework -U GRASS.applescript -o $(OBJDIR)/GRASS.scpt GRASS.applescript

$(OBJDIR)/grass.sh: grass.sh.in
	@test -d $(OBJDIR) || mkdir $(OBJDIR)
	@#-cp grass.sh $(OBJDIR)/grass.sh
	sed -e "s,@GDAL_BIN_PATH@,$(GDAL_BIN)," -e "s,@TCLTK_INTERNAL@,$(TCLTK_INTERNAL)," grass.sh.in > $(OBJDIR)/grass.sh
	@chmod a+x $(OBJDIR)/grass.sh

clean:
	-rm -rf $(OBJDIR) $(EXTRA_CLEAN_DIRS) $(EXTRA_CLEAN_FILES)
