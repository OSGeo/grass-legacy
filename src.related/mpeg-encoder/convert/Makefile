# 
# Copyright (c) 1995 The Regents of the University of California.
# All rights reserved.
# 
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose, without fee, and without written agreement is
# hereby granted, provided that the above copyright notice and the following
# two paragraphs appear in all copies of this software.
# 
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
# 

# compiler
CC = gcc
#CC = cc

# compiler flags
CFLAGS = -O -g -Wall -W -Wreturn-type -Wunused -Wmissing-prototypes $(PROFLAG)

# if you have an ANSI C Compiler, use the following line, otherwise
# uncomment the longer .c.o rule, also

.c.o:
	$(CC) $(CFLAGS) -c $<

#.c.o:
#	./ansi2knr $< tmpstupid.c
#	$(CC) $(CFLAGS) -c tmpstupid.c
#	mv tmpstupid.o $@
#	rm tmpstupid.c
#
#ANSI2KNR=ansi2knr

# set to null if you dont know what vid files are....
VID_SRC = 
#VID_SRC = vidtoppm vidtojpeg vidtoeyuv

# programs to be made
TARGETS = ppmtoeyuv eyuvtoppm eyuvtojpeg $(VID_SRC) jmovie2jpeg mpeg_demux 

# change this to be your include directory for header files
INCLUDE = -I/n/picasso/project/mm/xvideo/include

all: $(TARGETS)

ppmtoeyuv: ppmtoeyuv.c
	$(CC) -o ppmtoeyuv ppmtoeyuv.c

jmovie2jpeg: jmovie2jpeg.c
	$(CC) -o jmovie2jpeg jmovie2jpeg.c

mpeg_demux: mpeg_demux.c
	$(CC) -O -o mpeg_demux mpeg_demux.c
	
vidtoppm: vidtoppm.c
	$(CC) -lXvid -lX11 $(INCLUDE) -o vidtoppm vidtoppm.c 

vidtojpeg: vidtojpeg.c
	$(CC) -lXvid -lX11 $(INCLUDE) -o vidtojpeg vidtojpeg.c

vidtoeyuv: vidtoeyuv.c
	$(CC) -lXvid -lX11 $(INCLUDE) -o vidtoeyuv vidtoeyuv.c

wc:;		wc -l *.[ch] *.pl *.table
clean:;		rm -f *.o core *~ gmon.out $(TARGETS)
clobber:;   rm -f *.o core *~ gmon.out core tags $(TARGETS)
tags:;		ctags -t *.[ch]
