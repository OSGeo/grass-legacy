SHELL=/bin/sh
TOP=/export/grass.src/xgrass/src
GRASSSRC=/grass.src/4.1
GISLIBDIR=/grass.src/4.1/src/libes/LIB.sun4
GISBASE=/grass.src/4.1/sun4
INSTALLROOT=/export/grass.src/xgrass/src/install
XG_LIB_DIR=/export/grass.src/xgrass/src/install/lib
XG_INC_DIR=/export/grass.src/xgrass/src/install/include
XG_BIN_DIR=/export/grass.src/xgrass/src/install/bin
XG_HELP_DIR=/export/grass.src/xgrass/src/install/lib/help
EXTRA_LDFLAGS=
LDFLAGS=-O  
LIBS=-ll -L/export/grass.src/xgrass/src/install/lib -lXg  -lXm  -lXt  -lX11 -L/grass.src/4.1/src/libes/LIB.sun4 -lgis -lm
CFLAGS=-O -DSUNOS -D_NO_PROTO -DUSE_GRASS  -I. -I/export/grass.src/xgrass/src/install/include -I/grass.src/4.1/src/libes 
YFLAGS=-d
CC=cc
AR=ar
ARFLAGS=clq
RANLIB=ranlib
INSTALL=install
INSTDATFLAGS=-c -m 0444
RM=rm -f

all:: $(XG_BIN_DIR)/xgregion

OBJS = \
	xgregion.o 

SRC = \
	Makefile.SH \
	xgregion.c

$(XG_BIN_DIR)/xgregion: $(OBJS) $(XG_LIB_DIR)/libXg.a
	$(CC) -o $@ $(LDFLAGS) $(OBJS) $(LIBS) $(EXTRA_LDFLAGS)

Makefile:: Makefile.SH
	sh Makefile.SH

sparkling::
	$(RM) $(XG_BIN_DIR)/xgregion
	make clean

clean::
	$(RM) *.o *.a tags TAGS *.bak *.BAK *~ *% core errs ERRS \#* config.sh

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

