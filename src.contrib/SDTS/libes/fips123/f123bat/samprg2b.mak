CC = gcc -Wall -ansi -pedantic -I../f123inc

PGMNAME= ../f123app/samprg2b

SOURCE= ../f123app/samprg2b.c

OBJECTS= ../f123obj/samprg2b.o

LIB=../f123lib/fips123.a

FLAGS=

OTHERS=

all: setup $(PGMNAME)

setup:

$(OBJECTS): $(SOURCE)
	cd ../f123obj; $(CC)  -c $(SOURCE)

$(PGMNAME): $(OBJECTS)
	$(CC) $(OBJECTS) $(LIB) -o $(PGMNAME)
 
