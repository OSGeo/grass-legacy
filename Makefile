
MODULE_TOPDIR = .
include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

SUBDIRS = \
	lib \
	db \
	display \
	general \
	ps \
	raster \
	scripts \
	tools \
	vector \
	visualization

FILES = COPYING README 


BIN_DIST_FILES = $(FILES) \
	grass${VERSION_MAJOR}${VERSION_MINOR}.tmp \
	bin \
	bwidget \
	driver \
	etc \
	fonts \
	include \
	scripts 

default:
	@list='$(SUBDIRS)'; \
	for subdir in $$list; do \
		(cd $$subdir && make) || exit 1; \
	done
	${SHELL} -c "cp -f $(FILES) ${ARCH_DISTDIR}/ ; true"
	${SHELL} -c "cp -f ${ARCH_BINDIR}/grass${VERSION_MAJOR}${VERSION_MINOR} ${ARCH_DISTDIR}/grass${VERSION_MAJOR}${VERSION_MINOR}.tmp ; true"

mix:
	GRASS_PERL=${PERL} sh ./tools/link -old=$(GRASS50) -new=./ -conf=./tools/link.conf

copymix:
	GRASS_PERL=${PERL} sh ./tools/link -copy -old=$(GRASS50) -new=./ -conf=./tools/link.conf

mixclean:
	 ${SHELL} -c "find . -lname '*' -exec rm {} \; 2>/dev/null ; true"

# Copy binary modules
binmix:
	GRASS_PERL=${PERL} sh ./tools/cpbin -old=$(GRASS50)/dist.$(ARCH) -new=dist.$(ARCH) -conf=./tools/cpbin.conf

clean: 
	@list='$(SUBDIRS)'; \
	for subdir in $$list; do \
		(cd $$subdir && make clean) || exit 1; \
	done
	${SHELL} -c "rm -rf ${ARCH_DISTDIR}/bin/         2>/dev/null ; true"
	${SHELL} -c "rm -rf ${ARCH_DISTDIR}/bwidget/     2>/dev/null ; true"
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
