case $CONFIG in
'')
    if test ! -f config.sh; then
        ln ../config.sh . 2> /dev/null || \
        ln ../../config.sh . 2> /dev/null || \
        ln ../../../config.sh . 2> /dev/null || \
        ln ../../../../config.sh . 2> /dev/null || \
        ln ../../../../../config.sh . 2> /dev/null || \
        (echo "Can't find config.sh."; exit 1)
    fi
    . config.sh
    ;;
esac
: This forces SH files to create target in same directory as SH file.
: This is so that make depend always knows where to find SH derivatives.
case "$0" in
*/*) cd `expr X$0 : 'X\(.*\)/'` ;;
esac
echo "Extracting Makefile (with variable substitutions)"
: This section of the file will have variable substitutions done on it.
: Move anything that needs config subs from !NO!SUBS! section to !GROK!THIS!.
: Protect any dollar signs and backticks that you do not want interpreted
: by putting a backslash in front.  You may delete these comments.
if test -f Makefile
then
    mv -f Makefile Makefile.bak
fi
$spitshell >Makefile <<!GROK!THIS!
SHELL=/bin/sh
TOP=$top
GRASSSRC=$grasssrc
GISLIBDIR=$grasssrc/src/libes/LIB$grassarchext
GISBASE=$grassbase
INSTALLROOT=$installroot
XG_LIB_DIR=$installroot/lib
XG_INC_DIR=$installroot/include
XG_BIN_DIR=$installroot/bin
XG_HELP_DIR=$installroot/lib/help
EXTRA_LDFLAGS=$extraldflags
LDFLAGS=$ldflags 
LIBS=-ll -L$installroot/lib -lXg $xmlibdir $xmlib $xtlibdir $xtlib $xlibdir $xlib -L$grasssrc/src/libes/LIB$grassarchext -lgis -lm
CFLAGS=$cflags -I. -I$installroot/include $grassinc $xincs
YFLAGS=-d
CC=$cc
AR=$ar
ARFLAGS=$arflags
RANLIB=$ranlib
INSTALL=$install
INSTDATFLAGS=$instdatflags
RM=$rm

!GROK!THIS!

: In the following dollars and backticks do not need the extra backslash.
$spitshell >>Makefile <<'!NO!SUBS!'
all:: $(XG_BIN_DIR)/xgsupport

OBJS = \
	editcellhd.o \
	verify.o \
	askformat.o \
	pickone.o \
	xgsupport.o 

SRC = \
	Makefile.SH \
	xgsupport.h \
	askformat.c \
	editcellhd.c \
	pickone.c \
	verify.c \
	xgsupport.c

$(XG_BIN_DIR)/xgsupport: $(OBJS) $(XG_LIB_DIR)/libXg.a
	$(CC) -o $@ $(LDFLAGS) $(OBJS) $(LIBS) $(EXTRA_LDFLAGS)

Makefile:: Makefile.SH
	sh Makefile.SH

sparkling::
	$(RM) $(XG_BIN_DIR)/xgsupport
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

!NO!SUBS!
chmod 755 Makefile
$eunicefix Makefile
