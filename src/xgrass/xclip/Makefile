SHELL=/bin/sh
TOP=/export/xgrass/src
GRASSSRC=/grass.src/4.1
GISLIBDIR=/grass.src/4.1/src/libes/LIB.sun4
GISBASE=/grass.src/4.1/sun4
LOCALBIN=/usr/local/bin
INSTALLROOT=/xgrass/src/install
XG_LIB_DIR=/xgrass/src/install/lib
XG_INC_DIR=/xgrass/src/install/include
XG_BIN_DIR=/xgrass/src/install/bin
XG_HELP_DIR=/xgrass/src/install/lib/help
EXTRA_LDFLAGS=
LDFLAGS=-O 
CFLAGS=-O -DSUNOS -D_NO_PROTO -DUSE_GRASS  -I. -I/grass.src/4.1/src/libes 
CC=cc
AR=ar
ARFLAGS=clq
RANLIB=ranlib
INSTALL=install
INSTDATFLAGS=-c -m 0444
RM=rm -f

all:: 

SRC= \
	Makefile.SH \
	d.erase.xclip \
	d.font.xclip \
	d.frame.xclip \
	d.label.xclip \
	d.mon.xclip \
	d.rast.xclip \
	d.sites.xclip \
	d.title.xclip \
	d.vect.xclip \
	g.copygroup.xclip \
	g.copyicon.xclip \
	g.copylabels.xclip \
	g.copyrast.xclip \
	g.copyregion.xclip \
	g.copysites.xclip \
	g.copyvect.xclip \
	g.list.xclip \
	g.removegroup.xclip \
	g.removeicon.xclip \
	g.removelabels.xclip \
	g.removerast.xclip \
	g.removeregion.xclip \
	g.removesites.xclip \
	g.removevect.xclip \
	g.renamegroup.xclip \
	g.renameicon.xclip \
	g.renamelabels.xclip \
	g.renamerast.xclip \
	g.renameregion.xclip \
	g.renamesites.xclip \
	g.renamevect.xclip \
	r.clump.xclip \
	r.coin.xclip \
	r.compress.xclip \
	r.describe.xclip \
	r.neighbors.xclip \
	r.report.xclip

sparkling::

clean::

Makefiles::
	sh Makefile.SH

checkin::
	@(if test -d RCS; then \
	echo "attempting checkin..."; else \
	echo "oops...first time?...making RCS directory"; mkdir -p RCS; fi; \
	for i in $(SRC) ;\
	do (echo Working on $$i; EXISTS="0"; \
	if test -f RCS/$$i,v; then  EXISTS="1" ; fi; \
	if test "$$EXISTS" = "1"; then \
	ci -l0.0.0 -f -m"auto checkin: `date`" -t$(TOP)/config/ci.txt $$i; \
	chmod 664 $$i; \
	else \
	ci -l0.0 -f -m"auto checkin: `date`" -t$(TOP)/config/ci.txt $$i; \
	chmod 664 $$i; \
	fi;);  done);

