## The Makefile for libraries
## Should only have to change LIBNAME, DEST and CC.
##
LIBNAME = gctp
PRINTNAME = GCTP
#CC = gcc -fstrength-reduce -fpcc-struct-return -ansi -pedantic -Wall
CC      = cc
DEST	= /usr/local/lib
FLAGS   = -O
INCLUDE =
CFLAGS  = $(INCLUDE) $(FLAGS)

EXTHDRS	= /usr/include/math.h \
		/usr/include/stdio.h

HDRS	      = gctp.h \
		nad27.h \
		nad83.h \
		proto.h

LIBRARY	      = lib$(LIBNAME).a

LINTLIB       = llib-l$(LIBNAME).ln

MAKEFILE      = Makefile

OBJS	      = adjlz0.o \
		asinz0.o \
		blkdta.o \
		dmslz0.o \
		e0fnz0.o \
		e1fnz0.o \
		e2fnz0.o \
		e3fnz0.o \
		e4fnz0.o \
		gtrnz0.o \
		mlfnz0.o \
		msfnz0.o \
		paksz0.o \
		phi1z0.o \
		phi2z0.o \
		phi3z0.o \
		phi4z0.o \
		pj01z0.o \
		pj02z0.o \
		pj03z0.o \
		pj04z0.o \
		pj05z0.o \
		pj06z0.o \
		pj07z0.o \
		pj08z0.o \
		pj09z0.o \
		pj10z0.o \
		pj11z0.o \
		pj12z0.o \
		pj13z0.o \
		pj14z0.o \
		pj15z0.o \
		pj16z0.o \
		pj17z0.o \
		pj18z0.o \
		pj19z0.o \
		pj20z0.o \
		qsfnz0.o \
		spakz0.o \
		tsfnz0.o \
		unitz0.o \
		untfz0.o \
		xtra.o

PRINTER	      = linus

SRCS	      = adjlz0.c \
		asinz0.c \
		blkdta.c \
		dmslz0.c \
		e0fnz0.c \
		e1fnz0.c \
		e2fnz0.c \
		e3fnz0.c \
		e4fnz0.c \
		gtrnz0.c \
		mlfnz0.c \
		msfnz0.c \
		paksz0.c \
		phi1z0.c \
		phi2z0.c \
		phi3z0.c \
		phi4z0.c \
		pj01z0.c \
		pj02z0.c \
		pj03z0.c \
		pj04z0.c \
		pj05z0.c \
		pj06z0.c \
		pj07z0.c \
		pj08z0.c \
		pj09z0.c \
		pj10z0.c \
		pj11z0.c \
		pj12z0.c \
		pj13z0.c \
		pj14z0.c \
		pj15z0.c \
		pj16z0.c \
		pj17z0.c \
		pj18z0.c \
		pj19z0.c \
		pj20z0.c \
		qsfnz0.c \
		spakz0.c \
		tsfnz0.c \
		unitz0.c \
		untfz0.c \
		xtra.c

RCS_MESSAGE   = auto make check-in

all:		$(LIBRARY)

$(LIBRARY):	$(OBJS)
		@echo -n "Loading $(LIBRARY) ... "
		@rm -f $@
		@ar cru $(LIBRARY) $(OBJS)
		@ranlib $(LIBRARY)
		@echo "done"

ci::  	$(SRCS)
	@case '${MFLAGS}' in *[i]*) set +e;; esac;
	-for i in $(SRCS) $(HDRS); do \
		(cd ../lib/; ci -q -m"RCS_MESSAGE" -l $$i; chmod 664 $$i); \
	done

clean:;		@rm -f $(OBJS) $(LIBRARY) $(LINTLIB) tags *~ linted *.bak

depend:;
	(cd ../lib; mkmf -f $(MAKEFILE);\
	makedepend -- $(CFLAGS) -- $(SRCS))

extract:;	@ar xo $(DEST)/$(LIBRARY)
		@rm -f __.SYMDEF

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(LIBRARY)
		@echo Installing $(LIBRARY) in $(DEST)
		@install -m 644 $(LIBRARY) $(DEST)
		@ranlib $(DEST)/$(LIBRARY)

library:	$(LIBRARY)

lint: 	$(LINTLIB)

$(LINTLIB):	$(SRCS)
		@rm -f $@
		@lint -habcuz $(INCLUDE) -C$(LIBNAME) $(SRCS) | tee linted

print:;
		@rm -f $(PRINTNAME)
		@touch $(PRINTNAME)
		@for i in $(SRCS) $(HDRS) ;\
		do\
			expand -4 $$i | cat -n | pr -f -h $$i >> $(PRINTNAME); \
			echo $$i; \
		done
		@enscript -2r -G -P$(PRINTER) $(PRINTNAME)
		@rm -f $(PRINTNAME)

saber:;		saber
# saber is in athena path ; old: /mit/saber/$(MACHINE)bin/saber
saber_src:
	#load $(CFLAGS) $(SRCS)

saber_obj:
	#load $(OBJS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:         $(DEST)/$(LIBRARY)

$(DEST)/$(LIBRARY): $(SRCS) $(HDRS) $(EXTHDRS)
		@-ar xo $(DEST)/$(LIBRARY)
		@make -f $(MAKEFILE) DEST=$(DEST) install clean

# DO NOT DELETE THIS LINE -- make depend depends on it.

asinz0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
blkdta.o: /usr/include/stdio.h gctp.h proto.h
dmslz0.o: /usr/include/stdio.h gctp.h proto.h
e4fnz0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
gtrnz0.o: /usr/include/stdio.h gctp.h proto.h
mlfnz0.o: /usr/include/math.h
msfnz0.o: /usr/include/math.h
paksz0.o: /usr/include/stdio.h
phi1z0.o: /usr/include/math.h /usr/include/stdio.h gctp.h proto.h
phi2z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
phi3z0.o: /usr/include/math.h /usr/include/stdio.h
phi4z0.o: /usr/include/stdio.h /usr/include/math.h
pj01z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj02z0.o: /usr/include/stdio.h gctp.h proto.h nad27.h nad83.h
pj03z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj04z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj05z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj06z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj07z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj08z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj09z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj10z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj11z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj12z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj13z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj14z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj15z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj16z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj17z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj18z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj19z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
pj20z0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
qsfnz0.o: /usr/include/math.h
spakz0.o: /usr/include/stdio.h
tsfnz0.o: /usr/include/stdio.h /usr/include/math.h gctp.h proto.h
unitz0.o: /usr/include/stdio.h gctp.h proto.h
untfz0.o: /usr/include/stdio.h
