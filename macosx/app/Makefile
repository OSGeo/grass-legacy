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
MAKE_DIR_CMD = mkdir -p -m 755
INSTALL_BIN = ${INSTALL} -m 755

OBJS = Info.plist GRASS.scpt grass.sh main.o

ARCH_OBJS := $(foreach obj,$(OBJS),$(OBJDIR)/$(obj)) 

# ugly hack - assume tcl and tk in same prefix, and same as TCLTKLIBPATH minus 'lib'
ifndef TCLTKPREFIX
TCLTKPREFIX = `echo "${TCLTKLIBPATH}" | sed -e "s,-L,," -e "s,/lib *$$,,"`
endif
# ugly hack - assume gdal prefix starts with -L flag
ifndef GDAL_BIN
GDAL_BIN = `echo "${GDALLIBS}" | sed -e "s,-L,," -e "s,/lib.*$$,/bin,"`
endif
# ugly hack - extract path to PROJ bin from nad2bin
ifndef PROJ_BIN
PROJ_BIN = `echo "${NAD2BIN}" | sed -e "s,.=,," -e "s,/nad2bin,,"`
endif

default: macosxapp

macosxapp: PkgInfo app.icns English.lproj/MainMenu.nib build_html_user_index.sh build_gui_user_menu.sh $(ARCH_OBJS)
	-${MAKE_DIR_CMD} ${APPDIR}/MacOS
	$(CC) $(LDFLAGS) -o ${APPDIR}/MacOS/GRASS $(OBJDIR)/main.o $(XTRA_LDFLAGS)
	-${MAKE_DIR_CMD} ${APPDIR}/Resources/Scripts
	-${MAKE_DIR_CMD} ${APPDIR}/Resources/etc
	-${INSTALL_DATA} app.icns ${APPDIR}/Resources
	-tar cBf - English.lproj --exclude CVS | (cd ${APPDIR}/Resources ; tar xBf - ) 2>/dev/null
	-${INSTALL_DATA} $(OBJDIR)/Info.plist ${APPDIR}
	-${INSTALL_DATA} PkgInfo ${APPDIR}
	-${INSTALL_BIN} $(OBJDIR)/grass.sh ${APPDIR}/Resources
	-${INSTALL_DATA} $(OBJDIR)/GRASS.scpt ${APPDIR}/Resources/Scripts
	-${INSTALL_BIN} build_html_user_index.sh ${APPDIR}/Resources/etc
	-${INSTALL_BIN} build_gui_user_menu.sh ${APPDIR}/Resources/etc
	@# TclTk embedding:
ifdef OPENGL_X11
	-${MAKE_DIR_CMD} ${APPDIR}/Resources/bin
	-${INSTALL_BIN} ${TCLTKPREFIX}/bin/tclsh8.4 ${APPDIR}/Resources/bin
	-${INSTALL_BIN} ${TCLTKPREFIX}/bin/wish8.4 ${APPDIR}/Resources/bin
	-${MAKE_DIR_CMD} ${APPDIR}/Resources/lib
	-${INSTALL_BIN} ${TCLTKPREFIX}/lib/libtcl8.4.dylib ${APPDIR}/Resources/lib
	-${INSTALL_BIN} ${TCLTKPREFIX}/lib/libtk8.4.dylib ${APPDIR}/Resources/lib
	-cd ${TCLTKPREFIX}/lib ; tar cBf - tcl8.4 | (cd ${APPDIR}/Resources/lib ; tar xBf - ) 2>/dev/null
	-cd ${TCLTKPREFIX}/lib ; tar cBf - tk8.4 | (cd ${APPDIR}/Resources/lib ; tar xBf - ) 2>/dev/null
	-${MAKE_DIR_CMD} ${APPDIR}/Resources/include
	-${INSTALL_DATA} ${TCLTKPREFIX}/include/tcl.h ${APPDIR}/Resources/include
	-${INSTALL_DATA} ${TCLTKPREFIX}/include/tclDecls.h ${APPDIR}/Resources/include
	-${INSTALL_DATA} ${TCLTKPREFIX}/include/tclPlatDecls.h ${APPDIR}/Resources/include
	-${INSTALL_DATA} ${TCLTKPREFIX}/include/tk.h ${APPDIR}/Resources/include
	-${INSTALL_DATA} ${TCLTKPREFIX}/include/tkDecls.h ${APPDIR}/Resources/include
	-${INSTALL_DATA} ${TCLTKPREFIX}/include/tkPlatDecls.h ${APPDIR}/Resources/include
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
	osacompile -d -x -i /System/Library/Frameworks/AppleScriptKit.framework -o $(OBJDIR)/GRASS.scpt GRASS.applescript

$(OBJDIR)/grass.sh: grass.sh.in
	@test -d $(OBJDIR) || mkdir $(OBJDIR)
	sed -e "s,@GDAL_BIN_PATH@,$(GDAL_BIN)," -e "s,@PROJ_BIN_PATH@,$(PROJ_BIN)," -e "s,@TCLTK_INTERNAL@,$(TCLTK_INTERNAL)," grass.sh.in > $(OBJDIR)/grass.sh

clean:
	-rm -rf $(OBJDIR) $(EXTRA_CLEAN_DIRS) $(EXTRA_CLEAN_FILES)
