case $CONFIG in
'')
    if test ! -f config.sh; then
	ln ../config.sh . || \
	ln ../../config.sh . || \
	ln ../../../config.sh . || \
	ln ../../../../config.sh . || \
	ln ../../../../../config.sh . || \
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
$spitshell >Makefile <<!GROK!THIS!
SHELL=/bin/sh
TOP=$top
GRASSSRC=$grasssrc
GISLIBDIR=$grasssrc/src/libes/LIB$grassarchext
GISBASE=$grassbase
LOCALBIN=$localbin
INSTALLROOT=$installroot
XG_LIB_DIR=$installroot/lib
XG_INC_DIR=$installroot/include
XG_BIN_DIR=$installroot/bin
XG_HELP_DIR=$installroot/lib/help
EXTRA_LDFLAGS=$extraldflags
LDFLAGS=$ldflags
CFLAGS=$cflags -I. $grassinc $xincs
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

!NO!SUBS!
chmod 755 Makefile
$eunicefix Makefile
