# Replace /usr/local/X11/motif/lib with the path to your Motif lib's
# Replace /usr/local/X11/motif/include with the path to your Motif include's

LIBS = -L/usr/local/X11/motif/lib -lMrm -lXm -lXt -lX11
CFLAGS = -g -I/usr/include/Xm

test: test.o Table.o 
	$(CC) test.o Table.o $(LIBS) -o $@

