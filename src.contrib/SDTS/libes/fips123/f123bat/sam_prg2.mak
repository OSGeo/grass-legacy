CC = gcc -Wall -ansi -pedantic -I../f123inc

PGMNAME= ../f123app/sam_prg2

SOURCE= ../f123app/sam_prg2.c

OBJECTS= ../f123obj/sam_prg2.o

LIB=../f123lib/fips123.a

FLAGS=

OTHERS=

all: setup $(PGMNAME)

setup:

$(OBJECTS): $(SOURCE)
	cd ../f123obj; $(CC)  -c $(SOURCE)

$(PGMNAME): $(OBJECTS)
	$(CC) $(OBJECTS) $(LIB) -o $(PGMNAME)
 
