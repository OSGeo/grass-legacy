MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Platform.make

MACOSX_APP_NAME = GRASS-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.app
MACOSX_APPDIR = ${MACOSX_APP_NAME}/Contents/MacOS
INST_DIR_MACOSX = ${prefix}/${MACOSX_APPDIR}
MACOSX_SUPPDIR = /Library/GRASS/${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}
MACOSX_BUNDLE_PREFIX = ${GRASS_HOME}/macosx/dist/${MACOSX_APPDIR}
VER_MINOR_REL = `printf "%d0%d" ${GRASS_VERSION_MINOR} ${GRASS_VERSION_RELEASE}`
VER_DATE = `date +"%g%m%d"`
MAKE_DIR_CMD = mkdir -p -m 755
LN = /bin/ln -sf
LN_DIR = /bin/ln -sfh
# get python version - use python in path, should be config'd python
PYVER = `python -V 2>&1 | cut -d \  -f 2 | cut -d . -f 1,2`

# packagemaker location, options and behaviour changed in xcode 3
ifeq ($(findstring darwin8,$(ARCH)),darwin8)
XCODE_VER = xcode2
PACKAGEMAKER = /Developer/Tools/packagemaker
PKG_INST_DIR = ${prefix}/${MACOSX_APP_NAME}
else
XCODE_VER = xcode3
PACKAGEMAKER = /Developer/usr/bin/packagemaker
PKG_INST_DIR = ${prefix}
endif

SUBDIRS =
ifeq ($(MACOSX_APP),1)
SUBDIRS = app
endif

FILES = AUTHORS COPYING CHANGES REQUIREMENTS.html GPL.TXT

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: subdirs

include bundle.make

install-macosx:
	echo ${ARCH_DISTDIR}/${MACOSX_APP_NAME}
	@ if [ ! -d ${ARCH_DISTDIR}/${MACOSX_APP_NAME} ] ; then \
		echo "ERROR: GRASS.app has not been compiled. Try \"make\" in the top level source dir first."; \
		echo "  Installation aborted, exiting Make."; \
		exit; \
	fi; \
	${MAKE} real-install-macosx

# Duplicated, mostly, from the top-level makefile.

real-install-macosx: INST_DIR_TARGET = ${INST_DIR}
real-install-macosx: PREFIX_TARGET = ${prefix}
real-install-macosx: install-basic-macosx macosx-support-dir bundle-macosx
	echo ${ARCH_DISTDIR}/${MACOSX_APP_NAME}

# create a packagemaker installer package.
# pretty much the same as install, but needs intermediate install location
# to assemble package parts.

bindist-macosx: INST_DIR_TARGET = ${MACOSX_BUNDLE_PREFIX}
bindist-macosx: PREFIX_TARGET = ${GRASS_HOME}/macosx/dist
bindist-macosx: install-basic-macosx bundle-macosx
	@# installer pkg resources
	${MAKE_DIR_CMD} dist/resources
	-sed -e "s#@INST_DIR@#${INST_DIR}#g" -e "s#@MACOSX_SUPPDIR@#${MACOSX_SUPPDIR}#g" -e "s#@GRASS_VER@#${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}#g" -e "s#@PYVER@#${PYVER}#g" pkg/resources/postflight.in > dist/resources/postflight
	-chmod a+rx dist/resources/postflight
	-sed -e "s#@GRASS_VER@#${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}#g" pkg/resources/Description.plist.in > dist/Description.plist
	-sed -e "s#@GRASS_VERSION_MAJOR@#${GRASS_VERSION_MAJOR}#g" -e "s#@GRASS_VERSION_MINOR@#${GRASS_VERSION_MINOR}#g" -e "s#@GRASS_VERSION_RELEASE@#${GRASS_VERSION_RELEASE}#g" -e "s#@VER_MINOR_REL@#${VER_MINOR_REL}#g" -e "s#@VER_DATE@#${VER_DATE}#g" -e "s#@PKG_INST_DIR@#${PKG_INST_DIR}#g" pkg/resources/Info.plist.in > dist/Info.plist
	-${INSTALL_DATA} pkg/resources/License.rtf dist/resources
	-${INSTALL_DATA} pkg/resources/ReadMe.rtf dist/resources
	@# build the package
ifeq ($(XCODE_VER),xcode3)
	${PACKAGEMAKER} --target 10.4 --root-volume-only --id org.osgeo.grass --root dist/${MACOSX_APP_NAME} --info dist/Info.plist --out "GRASS-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.pkg" --resources dist/resources
	rm -f "GRASS-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.pkg/Contents/Resources/TokenDefinitions.plist"
else
	${PACKAGEMAKER} -build -ds -p "GRASS-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.pkg" -f dist/${MACOSX_APP_NAME} -r dist/resources -i dist/Info.plist -d dist/Description.plist
endif
	@# remove intermediate files
	@# -rm -rf dist 2>/dev/null

install-basic-macosx:
	${MAKE_DIR_CMD} ${INST_DIR_TARGET}
	-cd ${GISBASE} ; tar cBf - ${MACOSX_APP_NAME} | (cd ${PREFIX_TARGET} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - $(FILES) | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - bin | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - bwidget | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - docs | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - driver | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - etc | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - fonts | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - man | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - scripts | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - tools | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null
	if [ ${LOCALE} -eq 1 ] ; then cd ${GISBASE} ; tar cBf - locale | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null ; fi
	-cd ${GISBASE} ; tar cBf - include | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null
	-sed -e "s#^\(GRASS_HOME.[^=]*\).*#\1= ${INST_DIR}#" \
		-e "s#^\(RUN_GISBASE.[^=]*\).*#\1= ${INST_DIR}#" \
		-e "s#-isysroot /.*MacOSX.*\.sdk##g" \
		${GISBASE}/include/Make/Platform.make > ${INST_DIR_TARGET}/include/Make/Platform.make
	-sed -e "s#^\(ARCH_DISTDIR.[^=]*\).*#\1= ${INST_DIR}#" \
		-e "s#^\(ARCH_BINDIR.[^=]*\).*#\1= ${UNIX_BIN}#" \
		${GISBASE}/include/Make/Grass.make > ${INST_DIR_TARGET}/include/Make/Grass.make
	-cd ${GISBASE} ; tar cBf - lib | (cd ${INST_DIR_TARGET} ; tar xBf - ) 2>/dev/null
	-sed 's#'${GISBASE}'#'${INST_DIR_TARGET}'#g' ${GISBASE}/etc/monitorcap > ${INST_DIR_TARGET}/etc/monitorcap
	-chmod -R a+rX ${PREFIX_TARGET}/${MACOSX_APP_NAME} 2>/dev/null
	-cd ${GRASS_HOME} ; tar cBf - gem/skeleton | (cd ${INST_DIR_TARGET}/etc ; tar xBf - ) 2>/dev/null
	-${INSTALL} ${GRASS_HOME}/gem/gem6 ${INST_DIR_TARGET} 2>/dev/null
	-touch ${PREFIX_TARGET}/${MACOSX_APP_NAME}

macosx-support-dir:
	${MAKE_DIR_CMD} ${MACOSX_SUPPDIR}
	${MAKE_DIR_CMD} ${MACOSX_SUPPDIR}/Modules/bin
	${MAKE_DIR_CMD} ${MACOSX_SUPPDIR}/Modules/docs/html
	${MAKE_DIR_CMD} ${MACOSX_SUPPDIR}/Modules/etc
	${MAKE_DIR_CMD} ${MACOSX_SUPPDIR}/Modules/lib

clean: cleansubdirs
	-rm -rf ${ARCH_DISTDIR}/${MACOSX_APP_NAME}/ 2>/dev/null
