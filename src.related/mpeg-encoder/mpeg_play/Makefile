# Step 1:
#	Set CC to the C compiler you want to use.  On Sun, gcc 
#	produces faster code.  Your mileage may vary.
CC            = gcc
#CC            = cc

# Step 2:
#	Set INCLUDEDIR equal to -I followed by include directory
#	path for X11 include files. 

INCLUDEDIR    = -I/usr/include -I/usr/include/X11

# For Solaris/openwindows
#INCLUDEDIR = -I/usr/include -I/usr/openwin/share/include -I/usr/openwin/include -I/usr/dt/include

#
# Step 3:
#	Set CFLAGS.  Below are def's for some machines.  Uncomment the
#	appropriate one or make one of your own. If you want the player
#       to gather statistics about the video stream, add -DANALYSIS to
#       CFLAGS. If you do NOT want to use shared memory, remove 
#       -DSH_MEM from CFLAGS.
#       Use -DSIG_ONE_PARAM if the function passed to signal takes 
#         one parameter (linux, Solaris)
#         Do not use it if it takes none (SunOS, HPUX, Ultrix, OSF1, )
#         It works either way, (i think), just gets rid of a warning.
#
#       Use -NDEBUG for speed, -DDEBUG for resiliance.  On invalid MPEG
#         streams a DEBUG compiled mpeg_play will exit gracefully, while
#         a NDEBUG one will dump core.  But NDEBUG is faster.
#
#       Use -DQUIET to have mpeg_play print little by default (useful for
#         when it is called by another program like netscape).
#       Use -DQUALITY if you want the default to play streams that conform
#         strictly to the MPEG standard but take longer to display.
#       Use -DFLOATDCT to enable using the computationally expensive but
#         accurate floating point DCT through the -quality parameter.

# gcc
CFLAGS	      =  -O3 -DNDEBUG -DNONANSI_INCLUDES -DSH_MEM $(INCLUDEDIR) 

#Sun Solaris/Openwindows 
#CFLAGS = -O3 -DNDEBUG -DBSD -DNONANSI_INCLUDES -DSH_MEM -DSIG_ONE_PARAM $(INCLUDEDIR) 

#Linux C Flags
#CFLAGS	      =  -O3 -fomit-frame-pointer -ffast-math -finline-functions -m486 -DNDEBUG -DNONANSI_INCLUDES -DSH_MEM -DSIG_ONE_PARAM $(INCLUDEDIR) 

#HP C Flags 
#CFLAGS        = -Ac +O3 -DSH_MEM -DNDEBUG $(INCLUDEDIR) 

#DEC C Flags    (On some Alpha's you need -lbsd also)
#CFLAGS        = -O -DSH_MEM -DNDEBUG $(INCLUDEDIR)

#RS6000 C Flags -- Most RS6000's do not support shared memory,
# but we include it 'cuz it helps so much if you have it.
# by the way, if you want to give us a version of libXext with them, feel free
#CFLAGS        = -O -DSH_MEM $(INCLUDEDIR)

#SGI C Flags
#CFLAGS        = -O -cckr -DSH_MEM $(INCLUDEDIR)

#MIPGS RISC/os 4.5{1,2} C Flags
#CFLAGS        = -O -systype sysv -DSH_MEM -DNONANSI_INCLUDES -DMIPS

#PTX Flags (Dynix)
#CFLAGS        = -O -DNDEBUG $(INCLUDEDIR)

#NEWS C Flags
#CFLAGS	       = -O2 -DSh_MEM -DNO_LRAND48 -DNDEBUG -DNONANSI_INCUDES $(INCLUDEDIR)

#
#NeXT C Flags
#CFLAGS	       = -O -DNO_LRAND48 -DNDEBUG $(INCLUDEDIR)

#CETIA Unigraph/X C Flags
#CFLAGS	       = -O -DNDEBUGS -DBSDCOMPAT -DBSD_LARGE -DCETIA -DX_NOT_STDC_ENV $(INCLUDEDIR) 

#
# Step 4:
#	Set LIBS equal to path of libXext.a and libX11.a or the loader
#	flag equivalents (i.e. -lXext -lX11).
#       If you are NOT using shared memory, libXext.a is unecessary.
#       NOTE: below the default definition are
#             a few definitions for specific architectures.

LIBS	      = -L/usr/lib/X11 -lXext -lX11

# LIBS for MIPS RISC/os 4.5{1,2}
#LIBS         = -systype sysv -lXext -lX11 -lbsd

# LIBS for PTX/Dynix
#LIBS         = -lXext -lX11 -lsocket -linet -lnsl -lseq

# LIBS for CETIA Unigraph/X
#LIBS	      = -lX11 -lbsd

# LIBS for Solaris/Openwindows
#LIBS = -lXext -lX11 -lsocket -lnsl -lw -ldl -lelf -lucb

#
# Step 5:
#	Set DEST to pathname of final destination of player...
#
DEST	      = ~/bin


#
# Step 6 (System Dependant Stuff):
# On SOLARIS a user reports:

# Some systems are missing regcmp, to fix
# add -lgen to CFLAGS
# Also, uncomment the special SOLARIS INSTALL and LDFLAGS parameters.

# If you want it to run under pure VGA (on Linux)
# use the Linux CFLAGS, except kill -DSH_MEM, and
# LIBS	      =  -lX11 -lvga -lvgagl
# (you may also want to rename the execuable to mpeg_vga)
# and install the mpegvga patch in this directory

#
# That's it!  The rest of this shouldn't need any modifications...
#

EXTHDRS	      =

HDRS	      = util.h video.h decoders.h fs2.h dither.h fs4.h

INSTALL	      = /etc/install

#Solaris INSTALL
#INSTALL       = /usr/sbin/install

LD            = $(CC)

LDFLAGS       =

#Solaris LDFLAGS
#LDFLAGS       = -R/usr/openwin/lib:/usr/dt/lib -L/usr/openwin/lib -L/usr/dt/lib


MAKEFILE      = Makefile

OBJS          = util.o video.o parseblock.o motionvector.o decoders.o \
                fs2.o fs2fast.o fs4.o hybrid.o hybriderr.o 2x2.o floatdct.o\
                gdith.o gray.o mono.o main.o jrevdct.o 16bit.o util32.o\
                ordered.o ordered2.o mb_ordered.o readfile.o

PRINT	      = pr

PROGRAM       = mpeg_play

SHELL	      = /bin/sh

SRCS	      = util.c video.c parseblock.c motionvector.c decoders.c \
                main.c gdith.c fs2.c fs2fast.c fs4.c hybrid.c hybriderr.c \
                2x2.c gray.c mono.c jrevdct.c 16bit.c util32.c ordered.c \
                ordered2.c mb_ordered.c readfile.c floatdct.c

SYSHDRS	      = 

all:		$(PROGRAM)

$(PROGRAM):	$(OBJS)
		$(LD) $(LDFLAGS) $(OBJS) $(LIBS) -lm -o $(PROGRAM)
		strip $(PROGRAM)

clean:;		rm -f $(OBJS) core
		@make depend

clobber:;	rm -f $(OBJS) $(PROGRAM) core tags

depend:;	makedepend -- $(CFLAGS) -- $(SRCS)

echo:;		@echo $(HDRS) $(SRCS)

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@-strip $(PROGRAM)
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(PROGRAM); $(INSTALL) -f $(DEST) $(PROGRAM)); fi

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)









