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

OPENGLBASED = visualization

#compile if OPENGLBASED present:
ifneq ($(strip $(OPENGLLIB)),)
    SUBDIRS += $(OPENGLBASED)
endif

SUBDIRS = \
	lib \
	db \
	display \
	general \
	imagery \
	ps \
	raster \
	scripts \
	sites \
	tools \
	vector

FILES = COPYING README 


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
		(cd $$subdir && make) || exit 1; \
	done
	${SHELL} -c "cp -f $(FILES) ${ARCH_DISTDIR}/ ; true"
	${SHELL} -c "cp -f ${ARCH_BINDIR}/grass${VERSION_MAJOR}${VERSION_MINOR} ${ARCH_DISTDIR}/grass${VERSION_MAJOR}${VERSION_MINOR}.tmp ; true"
	@(cd tools ; sh -c "./build_html_index.html")

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

clean: 
	@list='$(SUBDIRS)'; \
	for subdir in $$list; do \
		(cd $$subdir && make clean) || exit 1; \
	done
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

distclean: clean
	${SHELL} -c "rm -f config.cache config.log config.status 2>/dev/null ; true"
	${SHELL} -c "rm -f include/config.h include/version.h include/winname.h include/Make/Grass.make include/Make/Platform.make 2>/dev/null ; true"

strip: FORCE
	@ if [ ! -f ${ARCH_BINDIR}/grass${VERSION_MAJOR}${VERSION_MINOR} ] ; then \
		echo "ERROR: Grass has not been compiled. Try \"make\" first."; \
		echo "  Strip aborted, exiting Make."; \
		exit; \
	fi; \
	${SHELL} -c "cd ${ARCH_DISTDIR} ; find . -type f -perm +111 -exec strip {} \; ; true"	

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
	${SHELL} -c "mkdir ./grass-${VERSION_MAJOR}${VERSION_MINOR}" ; true
	@ # needed to store code in package with grass-version path:
	${SHELL} -c "mv * ./grass-${VERSION_MAJOR}${VERSION_MINOR} " ; true
	@ #we use -h to get the linked files into as real files:
	tar cvfzh grass-${VERSION_MAJOR}${VERSION_MINOR}_src.tar.gz ./grass-${VERSION_MAJOR}${VERSION_MINOR}/* --exclude=CVS
	@ # restore src code location:
	${SHELL} -c "mv ./grass-${VERSION_MAJOR}${VERSION_MINOR}/* ." ; true
	${SHELL} -c "rmdir ./grass-${VERSION_MAJOR}${VERSION_MINOR}" ; true
	@ echo "Distribution source package: grass-${VERSION_MAJOR}${VERSION_MINOR}_src.tar.gz ready."


htmldocs:
	(cd lib/db/ ; make htmldocs)
	(cd lib/vector/ ; make htmldocs)
	#next runs only on grass${VERSION_MAJOR}${VERSION_MINOR}refman.dox (as defined in ./Doxyfile)
	doxygen ./Doxyfile

packagehtmldocs: htmldocs
	tar cvfz grass${VERSION_MAJOR}${VERSION_MINOR}refman_`date '+%Y_%m_%d'`.tar.gz doxygenhtml/ lib/db/html lib/vector/html

pdfdocs:
	(cd lib/db/ ; make pdfdocs)
	(cd lib/vector/ ; make pdfdocs)
