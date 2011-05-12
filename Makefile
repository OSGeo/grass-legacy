#############################################################################
#
# MODULE:   	GRASS Compilation
# AUTHOR(S):	Original author unknown - probably CERL
#   	    	Justin Hickey - Thailand - jhickey AT hpcc.nectec.or.th
#		Markus Neteler - Germany - neteler AT itc.it
#		Andreas Lange - Germany - Andreas.Lange AT Rhein-Main.de
#		Radim Blazek - Italy - blazek AT itc.it
# PURPOSE:  	It provides the commands necessary to compile, install,
#		clean, and uninstall GRASS
#		See INSTALL file for explanations.
# COPYRIGHT:    (C) 2002,2004 by the GRASS Development Team
#
#               This program is free software under the GNU General Public
#   	    	License (>=v2). Read the file COPYING that comes with GRASS
#   	    	for details.
#
#############################################################################

MODULE_TOPDIR = .

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

# Install directories
exec_prefix=            ${prefix}
BINDIR=			${UNIX_BIN}

# Shell commands
MAKE_DIR_CMD=		mkdir -p -m 755

SUBDIRS = \
	lib \
	db \
	display \
	doc \
	general \
	gui \
	imagery \
	misc \
	ps \
	raster \
	raster3d \
	scripts \
	sites \
	tools \
	vector \
	visualization \
	man \
	macosx

ifeq ($(strip $(MINGW)),)
	SUBDIRS += gem
endif

ifneq ($(strip $(HAVE_NLS)),)
	LOCALE=1
else
	LOCALE=0
endif

FILES = AUTHORS COPYING CHANGES REQUIREMENTS.html GPL.TXT contributors.csv  contributors_extra.csv  translators.csv

BIN_DIST_FILES = $(FILES) \
	grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}.tmp \
	bin \
	bwidget \
	docs \
	driver \
	etc \
	fonts \
	include \
	lib \
	man \
	scripts \
	tools

DOXNAME=grass

include $(MODULE_TOPDIR)/include/Make/Docs.make
include $(MODULE_TOPDIR)/include/Make/Doxygen.make

default: builddemolocation
	@echo "GRASS GIS compilation log"     > $(ERRORLOG)
	@echo "-------------------------"    >> $(ERRORLOG)
	@echo "Started compilation: `date`"  >> $(ERRORLOG)
	@echo "--"                           >> $(ERRORLOG)
	@echo "Errors in:"                   >> $(ERRORLOG)
	chmod 755 install-sh
	@list='$(SUBDIRS)'; \
	for subdir in $$list; do \
		$(MAKE) -C $$subdir; \
	done
	$(MAKE) manifests 
	if [ ${LOCALE} -eq 1 ] ; then $(MAKE) -C locale; fi
	-cp -f $(FILES) ${ARCH_DISTDIR}/
	-cp -f ${ARCH_BINDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR} ${ARCH_DISTDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}.tmp
	@test -d ${ARCH_DISTDIR}/tools/ || mkdir -p ${ARCH_DISTDIR}/tools/
	-${INSTALL} tools/mkhtml.sh ${ARCH_DISTDIR}/tools/ 2>/dev/null
	@test -d ${ARCH_DISTDIR}/tools/g.html2man/ || mkdir -p ${ARCH_DISTDIR}/tools/g.html2man/
	-${INSTALL} tools/g.html2man/g.html2man ${ARCH_DISTDIR}/tools/g.html2man/ 2>/dev/null
	@(cd tools ; sh -c "./build_html_index.sh")
	@if [ `cat "$(ERRORLOG)" | wc -l` -gt 5 ] ; then \
		echo "--"     >> $(ERRORLOG) ; \
		echo "In case of errors please change into the directory with error and run 'make'." >> $(ERRORLOG) ; \
		echo "If you get multiple errors, you need to deal with them in the order they"      >> $(ERRORLOG) ; \
		echo "appear in the error log. If you get an error building a library, you will"     >> $(ERRORLOG) ; \
		echo "also get errors from anything which uses the library."  >> $(ERRORLOG) ; \
	else \
		echo "No errors detected." >> $(ERRORLOG) ; \
	fi
	@echo "--"  >> $(ERRORLOG)
	@echo "Finished compilation: `date`" >> $(ERRORLOG)
	@cat $(ERRORLOG)
	@if [ `cat "$(ERRORLOG)" | wc -l` -gt 8 ] ; then false ; else true ; fi

manifests:
ifneq ($(strip $(MINGW)),)
	find $(ARCH_DISTDIR) -type f -name '*.exe' | \
	while read file ; do \
	    cmd=`basename "$$file" .exe` ; \
	    sed "s/@CMD@/$$cmd/" mswindows/generic.manifest > "$$file".manifest ; \
	done
endif

LIBDIRS = \
	lib/external/shapelib \
	lib/datetime \
	lib/gis \
	lib/linkm \
	lib/db \
	lib/form \
	lib/vector \
	db/drivers

# Compile libraries only
libs:
	make -C lib/ headers
	@list='$(LIBDIRS)'; \
	for subdir in $$list; do \
		$(MAKE) -C $$subdir; \
	done
	-cp -f $(FILES) ${ARCH_DISTDIR}/
	-cp -fr --parents include ${ARCH_DISTDIR}/

#we leave this target for a while so that people can easily upgrade (11/2004):
mix:
	@echo "NOTE: 'make mix' is no longer needed (changed 9 Nov 2004)"

mixclean:
	list='$(SUBDIRS)'; \
	find include -type l -exec rm {} \; 2>/dev/null; \
	for subdir in $$list; do \
		find $$subdir -type l -exec rm {} \; 2>/dev/null; \
	done
	-rm -f MIX 

# Any target that has a dependency on this target will be forced to be made.
# If we switch to GNU Make then this feature can be replaced with .PHONY
FORCE:

cleandistdirs: 
	-rm -rf ${ARCH_DISTDIR}/bin/         2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/bwidget/     2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/docs/        2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/driver/      2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/etc/         2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/fonts/       2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/include/     2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/lib/         2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/locale/      2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/man/         2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/scripts/     2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/demolocation/ 2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/tcltkgrass/  2>/dev/null
	-rm -rf ${ARCH_DISTDIR}/tools/       2>/dev/null
	-rm -f ${ARCH_DISTDIR}/AUTHORS ${ARCH_DISTDIR}/CHANGES ${ARCH_DISTDIR}/REQUIREMENTS.html ${ARCH_DISTDIR}/COPYING ${ARCH_DISTDIR}/GPL.TXT \
		${ARCH_DISTDIR}/contributors.csv ${ARCH_DISTDIR}/contributors_extra.csv  ${ARCH_DISTDIR}/translators.csv \
		${ARCH_DISTDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}.tmp 2>/dev/null
	-rmdir ${ARCH_DISTDIR}
	-rm -f ${ARCH_BINDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR} 2>/dev/null
	-rm -f ${ARCH_BINDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}.bat 2>/dev/null
	-rmdir ${ARCH_BINDIR}

# Clean out the strings extracted from scripts for translation
cleanscriptstrings:
	rm -f locale/scriptstrings/*.c 2>/dev/null

clean: cleandistdirs cleanscriptstrings cleandocs
	@list='$(SUBDIRS)'; \
	for subdir in $$list; do \
		$(MAKE) -C $$subdir clean; \
	done

libsclean: cleandistdirs
	@list='$(LIBDIRS)'; \
	for subdir in $$list; do \
		$(MAKE) -C $$subdir clean; \
	done

distclean: clean
	-rm -f config.cache config.log config.status config.status.${ARCH} 2>/dev/null
	-rm -f ChangeLog ChangeLog.bak $(ERRORLOG) grass.pc
	-rm -f include/config.h include/version.h include/winname.h include/Make/Grass.make 2>/dev/null
	-rm -f include/Make/Platform.make include/Make/Doxyfile_arch_html include/Make/Doxyfile_arch_latex 2>/dev/null

strip: FORCE
	@ if [ ! -f ${ARCH_BINDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR} ] ; then \
		echo "ERROR: GRASS has not been compiled. Try \"make\" first."; \
		echo "  Strip aborted, exiting Make."; \
		exit; \
	fi; \
	cd ${ARCH_DISTDIR} ; find . -type f -perm +111 -exec strip {} \;

install: FORCE
	@ # The following action MUST be a single action. That is, all lines
	@ # except the last line must have a backslash (\) at the end to
	@ # continue the statement. The reason for this is that Make does not
	@ # have an exit command thus, exit terminates the shell. However, 
	@ # Make creates a new shell for each action listed for a target.
	@ # Therefore, the only way exit will quit Make is if there is only
	@ # a single action for the target.
	@ # Check if grass has been compiled, if INST_DIR is writable, and if
	@ # grass is part of INST_DIR
	echo ${ARCH_BINDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}
	@ if [ ! -f ${ARCH_BINDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR} ] ; then \
		echo "ERROR: GRASS has not been compiled. Try \"make\" first."; \
		echo "  Installation aborted, exiting Make."; \
		exit; \
	fi; \
	if [ "${MACOSX_APP}" = "1" ] ; then \
		${MAKE} install-macosx; \
		exit; \
	fi; \
	INST_PATH=`dirname ${INST_DIR}`; \
	while [ ! -d $$INST_PATH ]; do \
		INST_PATH=`dirname $$INST_PATH`; \
	done; \
	if [ ! -d "${INST_DIR}" -a ! -w "$$INST_PATH" ] ; then \
		echo "ERROR: Directory $$INST_PATH is a parent directory of your"; \
		echo "  install directory ${INST_DIR} and is not writable."; \
		echo "  Perhaps you need root access."; \
		echo "  Installation aborted, exiting Make."; \
		exit; \
	fi; \
	if [ -d ${INST_DIR} -a ! -w "${INST_DIR}" ] ; then \
		echo "ERROR: Your install directory ${INST_DIR} is not writable."; \
		echo "  Perhaps you need root access."; \
		echo "  Installation aborted, exiting Make."; \
		exit; \
	fi; \
	result=`echo "${INST_DIR}" | awk '{ if ($$1 ~ /grass/) print $$1 }'`; \
	if [ "$$result" = "" ] ; then \
		echo "WARNING: Your install directory ${INST_DIR}"; \
		echo "  does not contain the word 'grass'."; \
		echo "  It is highly recommended that the word 'grass' be part"; \
		echo "  of your install directory to avoid conflicts."; \
		echo "  Do you want to continue? [y/n]"; \
		read ans; \
		ans=`echo "$$ans" | tr A-Z a-z`; \
		if [ "$$ans" != "y" ] ; then \
			echo "Installation aborted, exiting Make."; \
			exit; \
		fi; \
	fi; \
	${MAKE} real-install

real-install: FORCE
	test -d ${INST_DIR} || ${MAKE_DIR_CMD} ${INST_DIR}
	@##### test -d ${INST_DIR}/dev || ${MAKE_DIR_CMD} ${INST_DIR}/dev
	test -d ${BINDIR} || ${MAKE_DIR_CMD} ${BINDIR}
	-sed -e "s#GISBASE=.*#GISBASE=${INST_DIR}#" ${ARCH_BINDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR} > ${BINDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}
	-chmod a+x ${BINDIR}/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}
	-cd ${GISBASE} ; tar cBf - $(FILES) | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - bin | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - bwidget | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - docs | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - driver | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - etc | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - fonts | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
ifeq ($(strip $(MINGW)),)
	-cd ${GISBASE} ; tar cBf - man | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
endif
	-cd ${GISBASE} ; tar cBf - scripts | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - tools | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	if [ ${LOCALE} -eq 1 ] ; then cd ${GISBASE} ; tar cBf - locale | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; fi
	@ # The man, include, and lib could go to ${PREFIX}/ BUT if this is
	@ # done, then the corresponding uninstall instructions must delete
	@ # the grass files BY FILENAME NOT DIRECTORY!! Otherwise there is a
	@ # high risk of deleteing system files since PREFIX is defined by
	@ # default to be /usr/local
	@##### -cd ${GISBASE} ; tar cBf - man | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-cd ${GISBASE} ; tar cBf - include | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-sed -e "s#^\(GRASS_HOME.[^=]*\).*#\1= ${INST_DIR}#" -e "s#^\(RUN_GISBASE.[^=]*\).*#\1= ${INST_DIR}#" ${GISBASE}/include/Make/Platform.make > ${INST_DIR}/include/Make/Platform.make
	-sed -e "s#^\(ARCH_DISTDIR.[^=]*\).*#\1= ${INST_DIR}#" -e "s#^\(ARCH_BINDIR.[^=]*\).*#\1= ${UNIX_BIN}#" ${GISBASE}/include/Make/Grass.make > ${INST_DIR}/include/Make/Grass.make
	-sed -e 's#/tools/g.html2man/g.html2man#/tools/g.html2man#' ${GISBASE}/include/Make/Man.make > ${INST_DIR}/include/Make/Man.make
	-cd ${GISBASE} ; tar cBf - lib | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null
	-sed 's#'${GISBASE}'#'${INST_DIR}'#g' ${GISBASE}/etc/monitorcap > ${INST_DIR}/etc/monitorcap
	-sed 's#'${GISBASE}'#'${INST_DIR}'#g' ${GISBASE}/etc/fontcap > ${INST_DIR}/etc/fontcap
	@##### -chmod -R 1777 ${INST_DIR}/locks 2>/dev/null
	-chmod -R a+rX ${INST_DIR} 2>/dev/null
ifeq ($(strip $(MINGW)),)
	@#GEM installation
	-tar cBf - gem/skeleton | (cd ${INST_DIR}/etc ; tar xBf - ) 2>/dev/null
	-${INSTALL} gem/gem$(GRASS_VERSION_MAJOR)$(GRASS_VERSION_MINOR) ${BINDIR} 2>/dev/null
endif
	@# enable OSX Help Viewer
	@if [ "`cat include/Make/Platform.make | grep -i '^ARCH.*darwin'`" ] ; then /bin/ln -sfh "${INST_DIR}/docs/html" /Library/Documentation/Help/GRASS-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR} ; fi


install-strip: FORCE
	${MAKE} strip
	${MAKE} install

install-macosx: FORCE
	${MAKE} -C macosx install-macosx

bindist:  
	if [ "${MACOSX_APP}" = "1" ] ; then \
		${MAKE} bindist-macosx; \
		exit; \
	fi; \
	${MAKE} real-bindist

real-bindist:
	mkdir -p ${ARCH_DISTDIR}/etc/nad/src ; \
	    cp -f ${MODULE_TOPDIR}/lib/proj/*.lla ${ARCH_DISTDIR}/etc/nad/src ; true
	( date=`date '+%d_%m_%Y'`; cd ${ARCH_DISTDIR}; tar cBf - ${BIN_DIST_FILES} | gzip -fc > ../grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}-${ARCH}-$$date.tar.gz)
	-date=`date '+%d_%m_%Y'`; name=grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}-${ARCH}-$$date.tar.gz; \
            size=`ls -l $$name | awk '{print $$5}'`; \
	    sed -e "s/BIN_DIST_VERSION/${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}-${ARCH}-$$date/" \
	    -e "s/GRASSPRG_NAME/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}/" \
	    -e "s/SIZE_TAR_FILE/$$size/" -e "s#BIN_DIST_DIR#'${INST_DIR}'#" \
	    -e "s/ARCHITECTURE/${ARCH}/" \
	    -e "s/LD_LIBRARY_PATH_VAR/${LD_LIBRARY_PATH_VAR}/" \
	    -e "s/TEST_STR=/TEST_STR=executable/" \
	    -e "s#IMPORTANT.*#Generated from the binaryInstall.src file using the command make bindist#" \
	    -e "s/# executable shell.*//" -e "s/# make bindist.*//" \
	    binaryInstall.src > grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}-${ARCH}-$$date-install.sh ; \
	    chmod a+x grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}-${ARCH}-$$date-install.sh 2>/dev/null

bindist-macosx:
	${MAKE} -C macosx bindist-macosx

# make a source package for distribution:
srcdist: FORCE distclean
	-${MAKE_DIR_CMD} ./grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}

	@ # needed to store code in package with grass-version path:
	-mv * ./grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}
	@ # do not include the debian control files:
	-mv ./grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}/debian .
	@ #we use -h to get the linked files into as real files:
	tar cvfzh grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}.tar.gz ./grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}/* --exclude=CVS
	@ # restore src code location:
	-mv ./grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}/* .
	-rmdir ./grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}
	@ echo "Distribution source package: grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}.tar.gz ready. Calculating MD5 sum..."
	md5sum grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}.tar.gz > grass-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}.md5sum

# make a source package for library distribution:
srclibsdist: FORCE distclean
	-${MAKE_DIR_CMD} ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}

	@ # needed to store code in package with grass-version path:
	-cp -L * ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}
	-cp -rL tools ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}
	-cp -rL include ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}
	-cp -rL --parents lib/external/shapelib ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}
	-cp -rL --parents lib/datetime ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}
	-cp -rL --parents lib/db ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}
	-cp -rL --parents lib/gis ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}
	-cp -rL --parents lib/linkm ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}
	-cp -rL --parents lib/form ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}
	-cp -rL --parents lib/vector ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}

	-cp -rL --parents db/drivers ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}

	tar chvfz grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}.tar.gz ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}/* --exclude=CVS
	-rm -r ./grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}
	@ echo "Distribution source package: grass-lib-${GRASS_VERSION_MAJOR}.${GRASS_VERSION_MINOR}.${GRASS_VERSION_RELEASE}.tar.gz ready."

builddemolocation:
	test -d ${ARCH_DISTDIR} || ${MAKE_DIR_CMD} ${ARCH_DISTDIR}
	-tar cBf - demolocation | (cd ${ARCH_DISTDIR}/ ; tar xBfo - ) 2>/dev/null
	@ echo "GISDBASE: ${RUN_GISBASE}" > ${RUN_GISRC}
	@ echo "LOCATION_NAME: demolocation" >> ${RUN_GISRC}
	@ echo "MAPSET: PERMANENT" >> ${RUN_GISRC}
	@ echo "GRASS_DB_ENCODING: utf-8" >> ${RUN_GISRC}
	@ echo "DEBUG: 0" >> ${RUN_GISRC}
	@ echo "GRASS_GUI: text" >> ${RUN_GISRC}
