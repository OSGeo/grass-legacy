#CC = gcc -Wall -ansi -pedantic -I../f123inc
CC = gcc
COMPILE_FLAGS = -amsi -pedantic -I../f123inc
EXTRA_CFLAGS = -DFIPS_BSD -g

PGMNAME= /huge/Stig/sdts/bin/sdtsdump

SOURCE= sdtsdump.c

OBJ= sdtsdump.o

FIPSLIBDIR=../f123lib/fips123.a

FLAGS=

OTHERS=

all: setup $(PGMNAME)

setup:

$(OBJ): $(SOURCE)
	cd ../f123obj; $(CC)  -c $(SOURCE)

$(PGMNAME): $(OBJ) $(FIPSLIBDIR/f123lib.a
	$(CC) $(OBJ) $(FIPSLIBDIR)/f123lib.a  -o $(PGMNAME)
 
