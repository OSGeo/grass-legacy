case $CONFIG in
'')
	if test ! -f config.sh; then
	ln ../config.sh . || \
	ln ../../config.sh . || \
	ln ../../../config.sh . || \
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
# Makefile - Makefile for xgen
#
###########################################################################
# Xgen was developed by Kurt Buehler, while at the Center for Advanced
# Decision Support for Water and Environmental Systems (CADSWES),
# University of Colorado at Boulder and at the Indiana Water Resources
# Research Center (IWRRC),
# Purdue University for the U.S. Army Construction Engineering Research
# Laboratory in support of the Geographical Resources Analysis Support
# System (GRASS) software. The example scripts were developed by Ms.
# Christine Poulsen of USA-CERL, much thanks goes to her for her work.
#
# Permission to use, copy, modify and distribute without charge this
# software, documentation, etc. is granted, provided that this comment is
# retained, and that the names of Kurt Buehler, Christine Poulsen, CADSWES,
# IWRRC, the University of Colorado at Boulder, Purdue University,
# or USA-CERL are not used in advertising or publicity pertaining to
# distribution of the software without specific, written prior permission.
#
# The author disclaims all warranties with regard to this software,
# including all implied warranties of merchantability and fitness, in no
# event shall the author be liable for any special, indirect or
# consequential damages or any damages whatsoever resulting from loss of
# use, data or profits, whether in an action of contract, negligence or
# other tortious action, arising out of or in connection with the use or
# performance of this software.
############################################################################
SHELL=/bin/sh
LOCALBIN=$localbin
MANSRC=$mansrc
MANEXT=$manext
LIBS=-ll $xmlibdir -l${mipsos}Xm $xtlibdir -l${mipsos}Xt $xlibdir -l${mipsos}X11 $extraldflags -lm
CC=$cc
YACC=yacc
LEX=lex
LDFLAGS=$ldflags
CFLAGS=$cflags -I. $xincs
RM=$rm

!GROK!THIS!

: In the following dollars and backticks do not need the extra backslash.
$spitshell >>Makefile <<'!NO!SUBS!'

YFLAGS=-d

PUBLIC = xgen
MANPAGES = xgen.man
UTIL=Makefile

OBJ1 = addnode.o allcapture.o alloctypes.o button.o buttonname.o 
OBJ2 = buttonpush.o check.o clear.o client.o cmdboard.o command.o control.o 
OBJ3 = createobj.o creatshell.o destshell.o doactions.o docapture.o dodup.o 
OBJ4 = doerror.o doexec.o dointer.o dojobs.o donotice.o doset.o editor.o error.o
OBJ5 = exit.o expand.o expandobj.o expandvar.o grammar.o help.o index.o 
OBJ6 = initshell.o label.o lex.o list.o listchng.o listdata.o main.o menu.o 
OBJ7 = message.o msgboard.o multiline.o objectargs.o objectval.o parsecom.o 
OBJ8 = popup.o procmess.o pulldown.o resource.o sensitive.o 
OBJ9 = separator.o signal.o slider.o strpart.o table.o tableact.o textact.o 
OBJ10 = textentry.o toggle.o togglechng.o toggledata.o token.o unique.o 
OBJ11 = update.o warning.o yesno.o Matrix.o Clip.o

OBJS1 = $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ5) $(OBJ6) $(OBJ7) $(OBJ8)
OBJS2 = $(OBJ9) $(OBJ10) $(OBJ11)

OBJS = $(OBJS1) $(OBJS2)

SRC1 = addnode.c allcapture.c alloctypes.c button.c buttonname.c buttonpush.c 
SRC2 = check.c client.c clear.c cmdboard.c command.c control.c createobj.c 
SRC3 = creatshell.c destshell.c doactions.c docapture.c dodup.c 
SRC4 = doerror.c doexec.c dointer.c dojobs.c donotice.c doset.c editor.c error.c
SRC5 = exit.c expand.c expandobj.c expandvar.c grammar.c help.c index.c 
SRC6 = initshell.c label.c lex.c list.c listchng.c listdata.c main.c menu.c 
SRC7 = message.c msgboard.c multiline.c objectargs.c objectval.c parsecom.c 
SRC8 = popup.c procmess.c pulldown.c resource.c sensitive.c 
SRC9 = separator.c signal.c slider.c strpart.c table.c tableact.c textact.c 
SRC10 = textentry.c toggle.c togglechng.c toggledata.c token.c unique.c 
SRC11 = update.c warning.c yesno.c Matrix.c Clip.c

SRCS1 = $(SRC1) $(SRC2) $(SRC3) $(SRC4) $(SRC5) $(SRC6) $(SRC7) $(SRC8)
SRCS2 = $(SRC9) $(SRC10) $(SRC11)

SRCS = $(SRCS1) $(SRCS2)

DSRC1 = addnode.c allcapture.c alloctypes.c button.c buttonname.c buttonpush.c 
DSRC2 = check.c client.c clear.c cmdboard.c command.c control.c createobj.c 
DSRC3 = creatshell.c destshell.c doactions.c docapture.c dodup.c 
DSRC4 = doerror.c doexec.c dointer.c dojobs.c donotice.c doset.c editor.c.SH 
DSRC5 = error.c exit.c expand.c expandobj.c expandvar.c grammar.y help.c index.c
DSRC6 = initshell.c label.c lex.l list.c listchng.c listdata.c main.c menu.c 
DSRC7 = message.c msgboard.c multiline.c objectargs.c objectval.c parsecom.c 
DSRC8 = popup.c procmess.c pulldown.c resource.c sensitive.c 
DSRC9 = separator.c signal.c slider.c strpart.c table.c tableact.c textact.c 
DSRC10 = textentry.c toggle.c togglechng.c toggledata.c token.c unique.c 
DSRC11 = update.c warning.c yesno.c Matrix.c Clip.c Makefile.SH
DSRC12 = xgen.h xgtypes.h Clip.h ClipP.h Matrix.h MatrixP.h Setup

DSRCS1 = $(DSRC1) $(DSRC2) $(DSRC3) $(DSRC4) $(DSRC5) $(DSRC6) $(DSRC7)
DSRCS2 = $(DSRC8) $(DSRC9) $(DSRC10) $(DSRC11) $(DSRC12)

DSRCS = $(DSRCS1) $(DSRCS2)

all: $(PUBLIC) $(UTIL)

xgen: $(OBJS) 
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o xgen

saber_xgen: $(SRCS)
	#load $(CFLAGS) $(SRCS) -Bstatic $(LIBS)

install: $(PUBLIC)
	- mv $(LOCALBIN)/xgen $(LOCALBIN)/xgen.old
	- if test `pwd` != $(LOCALBIN); then cp $(PUBLIC) $(LOCALBIN); fi
	cd $(LOCALBIN); chmod 755 $(PUBLIC)
	- if test `pwd` != $(MANSRC); then \
for page in $(MANPAGES); do \
rm -f $(MANSRC)/../cat$(MANEXT)/`basename $$page .man`.$(MANEXT); \
cp $$page $(MANSRC)/`basename $$page .man`.$(MANEXT); \
done; \
fi

clean:
	$(RM) xgen *.o core TAGS tags *~ y.tab.h grammar.c lex.c

tags:
	ctags -w *.[ch]
	ctags -xw *.[ch] > TAGS

dist:
	tar cvf - $(DSRCS) | compress > ./xgen.tar.Z

!NO!SUBS!
chmod 755 Makefile
$eunicefix Makefile
