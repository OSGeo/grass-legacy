#############################################################################
#
# MODULE:   	GRASS Compilation
# AUTHOR(S):	Original author unknown - probably CERL
#   	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
#		Markus Neteler - Germany - neteler@itc.it
#		Andreas Lange - Germany - Andreas.Lange@Rhein-Main.de
#		Radim Blazek - Italy - blazek@itc.it
# PURPOSE:  	It provides the commands necessary to compile, install,
#		clean, and uninstall GRASS
#		See INSTALL file for explanations.
# COPYRIGHT:    (C) 2002 by the GRASS Development Team
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
INST_DIR=		${prefix}/grass${VERSION_MAJOR}${VERSION_MINOR}

# Shell commands
MAKE_DIR_CMD=		mkdir -p -m 755
MAKE=			make
INSTALL=    	    	cp


SUBDIRS = \
	lib \
	db \
	display \
	general \
	gui \
	imagery \
	ps \
	raster \
	scripts \
	sites \
	tools \
	vector \
	visualization

FILES = COPYING README REQUIREMENTS.html

BIN_DIST_FILES = $(FILES) \
	grass${VERSION_MAJOR}${VERSION_MINOR}.tmp \
	bin \
	bwidget \
	docs \
	driver \
	etc \
	fonts \
	include \
	lib \
	scripts 

default:
	@list='$(SUBDIRS)'; \
	for subdir in $$list; do \
		(cd $$subdir && $(MAKE) ) || exit 1; \
	done
	${SHELL} -c "cp -f $(FILES) ${ARCH_DISTDIR}/ ; true"
	${SHELL} -c "cp -f ${ARCH_BINDIR}/grass${VERSION_MAJOR}${VERSION_MINOR} ${ARCH_DISTDIR}/grass${VERSION_MAJOR}${VERSION_MINOR}.tmp ; true"
	@(cd tools ; sh -c "./build_html_index.html")

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
	@list='$(LIBDIRS)'; \
	for subdir in $$list; do \
		(cd $$subdir && $(MAKE) ) || exit 1; \
	done
	${SHELL} -c "cp -f $(FILES) ${ARCH_DISTDIR}/ ; true"
	${SHELL} -c "cp -fr --parents include ${ARCH_DISTDIR}/ ; true"

mix:
	GRASS_PERL=${PERL} sh ./tools/link -old=$(GRASS50) -new=./ -conf=./tools/link.conf

copymix:
	GRASS_PERL=${PERL} sh ./tools/link -copy -old=$(GRASS50) -new=./ -conf=./tools/link.conf

mixclean:
	 ${SHELL} -c "find . -type l -exec rm {} \; 2>/dev/null ; true"

# Copy binary modules
binmix:
	GRASS_PERL=${PERL} sh ./tools/cpbin -old=$(GRASS50)/dist.$(ARCH) -new=dist.$(ARCH) -conf=./tools/cpbin.conf

# Any target that has a dependency on this target will be forced to be made.
# If we switch to GNU Make then this feature can be replaced with .PHONY
FORCE:

cleandistdirs: 
	${SHELL} -c "rm -rf ${ARCH_DISTDIR}/bin/         2>/dev/null ; true"
	${SHELL} -c "rm -rf ${ARCH_DISTDIR}/bwidget/     2>/dev/null ; true"
	${SHELL} -c "rm -rf ${ARCH_DISTDIR}/docs/        2>/dev/null ; true"
	${SHELL} -c "rm -rf ${ARCH_DISTDIR}/driver/      2>/dev/null ; true"
	${SHELL} -c "rm -rf ${ARCH_DISTDIR}/etc/         2>/dev/null ; true"
	${SHELL} -c "rm -rf ${ARCH_DISTDIR}/fonts/       2>/dev/null ; true"
	${SHELL} -c "rm -rf ${ARCH_DISTDIR}/include/     2>/dev/null ; true"
	${SHELL} -c "rm -rf ${ARCH_DISTDIR}/lib/         2>/dev/null ; true"
	${SHELL} -c "rm -rf ${ARCH_DISTDIR}/scripts/     2>/dev/null ; true"
	${SHELL} -c "rm -f ${ARCH_DISTDIR}/README ${ARCH_DISTDIR}/COPYING ${ARCH_DISTDIR}/grass${VERSION_MAJOR}${VERSION_MINOR}.tmp 2>/dev/null ; true"
	${SHELL} -c "rmdir ${ARCH_DISTDIR} ; true"
	${SHELL} -c "rm -f ${ARCH_BINDIR}/grass${VERSION_MAJOR}${VERSION_MINOR} 2>/dev/null ; true"
	${SHELL} -c "rmdir ${ARCH_BINDIR} ; true"

clean: cleandistdirs
	@list='$(SUBDIRS)'; \
	for subdir in $$list; do \
		(cd $$subdir && $(MAKE) clean) || exit 1; \
	done

libsclean: cleandistdirs
	@list='$(LIBDIRS)'; \
	for subdir in $$list; do \
		(cd $$subdir && $(MAKE) clean) || exit 1; \
	done

distclean: clean
	${SHELL} -c "rm -f config.cache config.log config.status 2>/dev/null ; true"
	${SHELL} -c "rm -f include/config.h include/version.h include/winname.h include/Make/Grass.make include/Make/Platform.make 2>/dev/null ; true"

strip: FORCE
	@ if [ ! -f ${ARCH_BINDIR}/grass${VERSION_MAJOR}${VERSION_MINOR} ] ; then \
		echo "ERROR: GRASS has not been compiled. Try \"make\" first."; \
		echo "  Strip aborted, exiting Make."; \
		exit; \
	fi; \
	${SHELL} -c "cd ${ARCH_DISTDIR} ; find . -type f -perm +111 -exec strip {} \; ; true"	

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
	echo ${ARCH_BINDIR}/grass${VERSION_MAJOR}${VERSION_MINOR}
	@ if [ ! -f ${ARCH_BINDIR}/grass${VERSION_MAJOR}${VERSION_MINOR} ] ; then \
		echo "ERROR: GRASS has not been compiled. Try \"make\" first."; \
		echo "  Installation aborted, exiting Make."; \
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
	${SHELL} -c "sed -e \"s#^GISBASE.*#GISBASE=${INST_DIR}#\" ${ARCH_BINDIR}/grass${VERSION_MAJOR}${VERSION_MINOR} > ${BINDIR}/grass${VERSION_MAJOR}${VERSION_MINOR} ; true"
	${SHELL} -c "chmod a+x ${BINDIR}/grass${VERSION_MAJOR}${VERSION_MINOR} ; true"
	${SHELL} -c "cd ${GISBASE} ; tar cBf - bin | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; true"
	${SHELL} -c "cd ${GISBASE} ; tar cBf - bwidget | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; true"
	${SHELL} -c "cd ${GISBASE} ; tar cBf - docs | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; true"
	${SHELL} -c "cd ${GISBASE} ; tar cBf - driver | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; true"
	${SHELL} -c "cd ${GISBASE} ; tar cBf - etc | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; true"
	${SHELL} -c "cd ${GISBASE} ; tar cBf - fonts | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; true"
	${SHELL} -c "cd ${GISBASE} ; tar cBf - scripts | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; true"
	@##### ${SHELL} -c "cd ${GISBASE} ; tar cBf - locale | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; true"
	@ # The man, include, and lib could go to ${PREFIX}/ BUT if this is
	@ # done, then the corresponding uninstall instructions must delete
	@ # the grass files BY FILENAME NOT DIRECTORY!! Otherwise there is a
	@ # high risk of deleteing system files since PREFIX is defined by
	@ # default to be /usr/local
	@##### ${SHELL} -c "cd ${GISBASE} ; tar cBf - man | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; true"
	${SHELL} -c "cd ${GISBASE} ; tar cBf - include | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; true"
	${SHELL} -c "cd ${GISBASE} ; tar cBf - lib | (cd ${INST_DIR} ; tar xBf - ) 2>/dev/null ; true"
	${SHELL} -c "sed 's#'${GISBASE}'#'${INST_DIR}'#g' ${GISBASE}/etc/monitorcap > ${INST_DIR}/etc/monitorcap ; true"
	@##### ${SHELL} -c "chmod -R 1777 ${INST_DIR}/locks 2>/dev/null ; true"
	${SHELL} -c "chmod -R a+rX ${INST_DIR} 2>/dev/null ; true"

install-strip: FORCE
	${MAKE} strip
	${MAKE} install


bindist:  
	( date=`date '+%d_%m_%Y'`; cd ${ARCH_DISTDIR}; tar cBf - ${BIN_DIST_FILES} | gzip -fc > ../grass${VERSION_MAJOR}${VERSION_MINOR}-$$date-${ARCH}-bin.tar.gz)
	date=`date '+%d_%m_%Y'`; name=grass${VERSION_MAJOR}${VERSION_MINOR}-$$date-${ARCH}-bin.tar.gz; \
            size=`ls -l $$name | awk '{print $$5}'`; \
	    sed -e "s/BIN_DIST_VERSION/${VERSION_MAJOR}${VERSION_MINOR}-$$date/" \
	    -e "s/SIZE_TAR_FILE/$$size/" -e "s#BIN_DIST_DIR#'${INST_DIR}'#" \
	    -e "s/ARCHITECTURE/${ARCH}/" \
	    -e "s/TEST_STR=/TEST_STR=executable/" \
	    -e "s#IMPORTANT.*#Generated from the binaryInstall.src file using the command make bindist#" \
	    -e "s/# executable shell.*//" -e "s/# make bindist.*//" \
	    binaryInstall.src > grass${VERSION_MAJOR}${VERSION_MINOR}-$$date-${ARCH}-install.sh ; \
	    chmod a+x grass${VERSION_MAJOR}${VERSION_MINOR}-$$date-${ARCH}-install.sh 2>/dev/null ; true

# make a source package for distribution (we include the 5.3.0 stuff):
srcdist: FORCE distclean
	${SHELL} -c "${MAKE_DIR_CMD} ./grass-${VERSION_MAJOR}${VERSION_MINOR}" ; true
	@ # needed to store code in package with grass-version path:
	${SHELL} -c "mv * ./grass-${VERSION_MAJOR}${VERSION_MINOR} " ; true
	@ #we use -h to get the linked files into as real files:
	tar cvfzh grass-${VERSION_MAJOR}${VERSION_MINOR}_src.tar.gz ./grass-${VERSION_MAJOR}${VERSION_MINOR}/* --exclude=CVS
	@ # restore src code location:
	${SHELL} -c "mv ./grass-${VERSION_MAJOR}${VERSION_MINOR}/* ." ; true
	${SHELL} -c "rmdir ./grass-${VERSION_MAJOR}${VERSION_MINOR}" ; true
	@ echo "Distribution source package: grass-${VERSION_MAJOR}${VERSION_MINOR}_src.tar.gz ready."

# make a source package for library distribution (we include the 5.3.0 stuff):
srclibsdist: FORCE distclean
	${SHELL} -c "${MAKE_DIR_CMD} ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR}" ; true
	echo "" > ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR}/SRCPKG
     
	@ # needed to store code in package with grass-version path:
	${SHELL} -c "cp -L * ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR} " ; true
	${SHELL} -c "cp -rL tools ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR} " ; true
	${SHELL} -c "cp -rL include ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR} " ; true
	${SHELL} -c "cp -rL --parents lib/external/shapelib ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR} " ; true
	${SHELL} -c "cp -rL --parents lib/datetime ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR} " ; true
	${SHELL} -c "cp -rL --parents lib/db ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR} " ; true
	${SHELL} -c "cp -rL --parents lib/gis ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR} " ; true
	${SHELL} -c "cp -rL --parents lib/linkm ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR} " ; true
	${SHELL} -c "cp -rL --parents lib/form ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR} " ; true
	${SHELL} -c "cp -rL --parents lib/vector ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR} " ; true

	${SHELL} -c "cp -rL --parents db/drivers ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR} " ; true
        
	tar cvfz grass-lib-${VERSION_MAJOR}${VERSION_MINOR}_src.tar.gz ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR}/* --exclude=CVS
	${SHELL} -c "rm -r ./grass-lib-${VERSION_MAJOR}${VERSION_MINOR}" ; true
	@ echo "Distribution source package: grass-lib-${VERSION_MAJOR}${VERSION_MINOR}_src.tar.gz ready."

htmldocs:
	(cd lib/db/ ; $(MAKE) htmldocs)
	(cd lib/vector/ ; $(MAKE) htmldocs)
	#next runs only on grass${VERSION_MAJOR}${VERSION_MINOR}refman.dox (as defined in ./Doxyfile)
	doxygen ./Doxyfile

packagehtmldocs: htmldocs
	tar cvfz grass${VERSION_MAJOR}${VERSION_MINOR}refman_`date '+%Y_%m_%d'`.tar.gz doxygenhtml/ lib/db/html lib/vector/html

pdfdocs:
	(cd lib/db/ ; $(MAKE) pdfdocs)
	(cd lib/vector/ ; $(MAKE) pdfdocs)

changelog:
	@ echo "creating ChangeLog file..."
	@ # cvs2cl.pl creates a GNU style ChangeLog file:
	@ # http://www.red-bean.com/cvs2cl
	GRASS_PERL=${PERL} sh tools/cvs2cl.pl -f ./ChangeLog
