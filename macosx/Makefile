MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Dir.make

MACOSX_APP_NAME = GRASS-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.app
MACOSX_APPDIR = ${MACOSX_APP_NAME}/Contents/Resources
INST_DIR_MACOSX = ${prefix}/${MACOSX_APPDIR}
SUBDIRS = 

ifeq ($(MACOSX_APP),1)
SUBDIRS = app modbuild
endif

default: subdirs

install-macosx:
	echo ${ARCH_BINDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}
	@ if [ ! -f ${ARCH_BINDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR} ] ; then \
		echo "ERROR: GRASS has not been compiled. Try \"make\" in the top level source dir first."; \
		echo "  Installation aborted, exiting Make."; \
		exit; \
	fi; \
	${MAKE} real-install-macosx

real-install-macosx:
	@# Duplicated, mostly, from the top-level makefile.  There are some
	@# differences, and it makes it easy to backport to earlier versions of
	@# GRASS where the main makefile doesn't know about macosx.
	-cd ${GISBASE} ; tar cBf - ${MACOSX_APP_NAME} | (cd ${prefix} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - bin | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - bwidget | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - docs | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - driver | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - etc | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - fonts | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - man | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - scripts | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	if [ ${LOCALE} -eq 1 ] ; then cd ${GISBASE} ; tar cBf - locale | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; fi
	-cd ${GISBASE} ; tar cBf - include | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - lib | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-sed 's#'${GISBASE}'#'${INST_DIR}'#g' ${GISBASE}/etc/monitorcap > ${INST_DIR}/etc/monitorcap
	-chmod -R a+rX ${INST_DIR} 2>/dev/null
	-cd ${GRASS_HOME} ; tar cBf - gem/skeleton | (cd ${INST_DIR}/etc ; tar xBf - ) 2>/dev/null
	-${INSTALL} ${GRASS_HOME}/gem/gem6 ${prefix}/${MACOSX_APP_NAME}/Contents/MacOS 2>/dev/null
	@### help file package - in the future will go in /Library/Documentation/Help

bindist-macosx:
	@# OSX bindist is essentially the same as an install, and doesn't need an install script.  no tarball generated.
	-cd ${GISBASE} ; tar cBf - ${MACOSX_APP_NAME} | (cd ${GRASS_HOME}/macosx ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - bin | (cd ${GRASS_HOME}/macosx/${MACOSX_APPDIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - bwidget | (cd ${GRASS_HOME}/macosx/${MACOSX_APPDIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - docs | (cd ${GRASS_HOME}/macosx/${MACOSX_APPDIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - driver | (cd ${GRASS_HOME}/macosx/${MACOSX_APPDIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - etc | (cd ${GRASS_HOME}/macosx/${MACOSX_APPDIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - fonts | (cd ${GRASS_HOME}/macosx/${MACOSX_APPDIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - man | (cd ${GRASS_HOME}/macosx/${MACOSX_APPDIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - scripts | (cd ${GRASS_HOME}/macosx/${MACOSX_APPDIR} ; tar xBf - ) 2>/dev/null
	if [ ${LOCALE} -eq 1 ] ; then cd ${GISBASE} ; tar cBf - locale | (cd ${GRASS_HOME}/macosx/${MACOSX_APPDIR} ; tar xBf - ) 2>/dev/null ; fi
	-cd ${GISBASE} ; tar cBf - include | (cd ${GRASS_HOME}/macosx/${MACOSX_APPDIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - lib | (cd ${GRASS_HOME}/macosx/${MACOSX_APPDIR} ; tar xBf - ) 2>/dev/null
	-sed 's#'${GISBASE}'#'${INST_DIR_MACOSX}'#g' ${GISBASE}/etc/monitorcap > ${GRASS_HOME}/macosx/${MACOSX_APPDIR}/etc/monitorcap
	-chmod -R a+rX ${GRASS_HOME}/macosx/${MACOSX_APPDIR} 2>/dev/null
	-cd ${GRASS_HOME} ; tar cBf - gem/skeleton | (cd ${GRASS_HOME}/macosx/${MACOSX_APPDIR}/etc ; tar xBf - ) 2>/dev/null
	-${INSTALL} ${GRASS_HOME}/gem/gem6 ${GRASS_HOME}/macosx/${MACOSX_APP_NAME}/Contents/MacOS 2>/dev/null

clean: cleansubdirs
	-rm -rf ${ARCH_DISTDIR}/${MACOSX_APP_NAME}/ 2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/modbuild/ 2>/dev/null
