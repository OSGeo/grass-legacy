# set proper install for local system
INSTALL = /etc/install

#LOCAL = /usr/local
LOCAL = $(GRAPH)/src

CFLAGS = -O -I$(LOCAL)/include

ftab:	ftab.o $(LOCAL)/lib/libgraph.a
	cc -o ftab ftab.o $(LOCAL)/lib/libgraph.a -lm

ut2:	ut2.o $(LOCAL)/lib/libgraph.a
	cc -o ut2 ut2.o $(LOCAL)/lib/libgraph.a -lm

cur:	cur.o $(LOCAL)/lib/libgraph.a
	cc -o cur cur.o $(LOCAL)/lib/libgraph.a -lm

boxes:	boxes.o $(LOCAL)/lib/libgraph.a
	cc -o boxes boxes.o $(LOCAL)/lib/libgraph.a -lm

install:
	make ftab boxes
	strip ftab boxes
	mv ftab boxes $(GRAPH)/bin
	rm -f *.o

