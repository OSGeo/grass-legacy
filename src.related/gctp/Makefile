# phil's makefile
# $Header: /mit/crlprojects/gctp/RCS/Makefile,v 1.1 90/10/19 15:05:09 phils Exp Locker: phils $

# Look in the user's environment for $MACHTYPE if not defined by make.
# MACHINE       = $${MACHTYPE}
# basename for library and lint - should match $(LIBDIR)/Makefile
LIBNAME       = gctp
CC            = cc
## CC            = gcc -Wall -ansi -pedantic
#DEST          = /mit/crlprojects/$(MACHINE)bin
DEST          = /usr/local/bin
#LIBDIR        = $(MACHINE)lib
LIBDIR        = $(MACHINE)lib
#INCLUDE       = -I$(MACHINE)lib
FLAGS         = -O
CFLAGS        = $(INCLUDE) $(FLAGS)
LDFLAGS       = -s
EXTLIBS	  	  = -L$(LIBDIR) -l$(LIBNAME) -lm
PROGRAM       = gctp

# Shouldn't have to touch anything below this line
#
EXTHDRS   = /usr/include/stdio.h \
		/usr/include/sys/file.h

HDRS      =

LIBS      = $(LIBDIR)/lib$(LIBNAME).a
LINTLIBS  = $(LIBDIR)/llib-l$(LIBNAME).ln

MAKEFILE  = Makefile

OBJS      = main.o

PRINTER   = linus  # ln03-9-551

RCS_LOG   = automatic make checkin

SRCS      = main.c

VPATH     = .

all:	$(PROGRAM)

allnew:		spotless program lint

$(PROGRAM): $(OBJS) $(LIBS)
	@echo -n "Linking $(PROGRAM) ... "
	@$(CC) $(LDFLAGS) $(OBJS) $(EXTLIBS) -o $(PROGRAM).out
	@mv $(PROGRAM).out $(PROGRAM)
	@echo "done"

$(LIBS):
	(cd $(LIBDIR); make $(MFLAGS) CC='$(CC)' MACHINE=$(MACHINE) \
	FLAGS='$(FLAGS)' LIBNAME=$(LIBNAME) library)

$(LINTLIBS):
	(cd $(LIBDIR); make $(MFLAGS) LIBNAME=$(LIBNAME) lint)

ci:  $(HDRS) $(SRCS)
	@ci -l -q -m"$(RCS_LOG)" $(HDRS) $(SRCS)
	@chmod g+w $(HDRS) $(SRCS)

clean:;		@rm -f $(OBJS) core *.o tags linted *~ *.BAK *.bak

spotless:	clean
	(cd $(LIBDIR); make $(MFLAGS) LIBNAME=$(LIBNAME) clean)
	@rm -f $(PROGRAM) install

depend:;
	@mkmf -a -F $(MAKEFILE) "PROGRAM=$(PROGRAM)"
	@makedepend -- $(INCLUDE) -- $(SRCS)

index:;		@ctags -wx $(HDRS) $(SRCS)


install: $(PROGRAM)
		install -s -m 755 $(PROGRAM) $(BINDIR)
		@touch install

lint:	$(LINTLIBS)
		@lint -habcuz $(INCLUDE) $(SRCS) $(LINTLIBS) -lm | tee linted

print:;
		@rm -f XIM
		@touch XIM
		@for i in $(SRCS) $(HDRS); do \
        	expand -4 $$i | cat -n | pr -f -h $$i >> XIM; \
            echo $$i; \
        done
		@enscript -2r -G -P$(PRINTER) XIM
		@rm -f XIM

program:	$(PROGRAM)

saber:
	/mit/saber/$(MACHINE)bin/saber

saber_src:
	#load $(CFLAGS) $(SRCS) $(LIBDIR)/*.c $(EXTLIBS)

saber_obj:
	#load $(OBJS) $(LIBDIR)/*.o $(EXTLIBS)

tags:		$(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

tar:
	tar cvf $(PROGRAM).tar $(HDRS) $(SRCS) $(MAKEFILE)\
	$(LIBDIR)/*.c $(LIBDIR)/*.h $(LIBDIR)/$(MAKEFILE)


# DO NOT DELETE THIS LINE -- make depend depends on it.

main.o: /usr/include/stdio.h /usr/include/sys/file.h lib/gctp.h
main.o: lib/proto.h
