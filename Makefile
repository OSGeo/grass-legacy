
MODULE_TOPDIR = .
include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

SUBDIRS = \
	lib \
	db \
	display \
	general \
	vector \
	visualization

default:
	@list='$(SUBDIRS)'; \
	for subdir in $$list; do \
		(cd $$subdir && make) || exit 1; \
	done

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
	${SHELL} -c "rmdir ${ARCH_DISTDIR} ; true"
	${SHELL} -c "rm -f ${ARCH_BINDIR}/grass${VERSION_NAME} 2>/dev/null ; true"
	${SHELL} -c "rmdir ${ARCH_BINDIR} ; true"

distclean: clean
	${SHELL} -c "rm -f config.cache config.log config.status 2>/dev/null ; true"
	${SHELL} -c "rm -f include/Make/Platform.make 2>/dev/null ; true"

