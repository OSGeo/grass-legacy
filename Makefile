
MODULE_TOPDIR = .
include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

SUBDIRS = \
	lib \
	db \
	display \
	general \
	scripts \
	vector \
	visualization

FILES = COPYING README 


BIN_DIST_FILES = $(FILES) \
	grass51.tmp \
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
	${SHELL} -c "cp -f ${ARCH_BINDIR}/grass51 ${ARCH_DISTDIR}/grass51.tmp ; true"

mix:
	./tools/link -old=$(GRASS50) -new=./ -conf=./tools/link.conf

copymix:
	./tools/link -copy -old=$(GRASS50) -new=./ -conf=./tools/link.conf

mixclean:
	 ${SHELL} -c "find . -lname '*' -exec rm {} \; 2>/dev/null ; true"

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
	${SHELL} -c "rmdir ${ARCH_DISTDIR} ; true"
	${SHELL} -c "rm -f ${ARCH_BINDIR}/grass${VERSION_NAME} 2>/dev/null ; true"
	${SHELL} -c "rmdir ${ARCH_BINDIR} ; true"

distclean: clean
	${SHELL} -c "rm -f config.cache config.log config.status 2>/dev/null ; true"
	${SHELL} -c "rm -f include/Make/Platform.make 2>/dev/null ; true"

bindist:  
	( date=`date '+%m%d%y'`; cd ${ARCH_DISTDIR}; tar cBf - ${BIN_DIST_FILES} | gzip -fc > ../grass51-$$date-${ARCH}-bin.tar.gz)
	date=`date '+%m%d%y'`; name=grass51-$$date-${ARCH}-bin.tar.gz; \
            size=`ls -l $$name | awk '{print $$5}'`; \
	    sed -e "s/BIN_DIST_VERSION/51-$$date/" \
	    -e "s/SIZE_TAR_FILE/$$size/" -e "s#BIN_DIST_DIR#'${INST_DIR}'#" \
	    -e "s/ARCHITECTURE/${ARCH}/" \
	    -e "s/TEST_STR=/TEST_STR=executable/" \
	    -e "s#IMPORTANT.*#Generated from the binaryInstall.src file using the command make bindist#" \
	    -e "s/# executable shell.*//" -e "s/# make bindist.*//" \
	    binaryInstall.src > grass51-$$date-${ARCH}-install.sh ; \
	    chmod a+x grass51-$$date-${ARCH}-install.sh 2>/dev/null ; true

htmldocs:
	(cd lib/db/ ; make htmldocs)
	(cd lib/vector/ ; make htmldocs)
	#next runs only on grass51refman.dox (as defined in ./Doxyfile)
	doxygen

packagehtmldocs:
	tar cvfz grass51refman_`date '+%Y_%m_%d'`.tar.gz doxygenhtml/ lib/db/html lib/vector/html

pdfdocs:
#	(cd lib/db/ ; make pdfdocs)
	(cd lib/vector/ ; make pdfdocs)
