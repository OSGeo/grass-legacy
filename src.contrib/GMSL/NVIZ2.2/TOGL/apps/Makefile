# updated tp tcl/tk 8.0  8/99 MN
# Makefile for Togl demos (version 1.0)

# The C compiler:
CC = cc

# Compiler options:
#COPTS = -c -O -cckr
COPTS = -c -O

TOGL_DIR = ..

# Where to find tcl.h, tk.h, OpenGL/Mesa headers, etc:
INCLUDES = -I/usr/include -I/usr/include/X11 -I/usr/local/include -I/usr/include/GL -I$(TOGL_DIR)
#INCLUDES = -I/usr/local/include
#INCLUDES = -I/usr2/people/brianp/Mesa/include -I/usr/local/include

# Where to find libtcl.a, libtk.a, OpenGL/Mesa libraries:
LIBDIRS = -L/usr/local/lib -L/usr/X11R6/lib
#LIBDIRS = -L/usr/local/lib -L/usr2/people/brianp/Mesa/lib 

# Libraries to link with:
LIBS = -ltk8.0 -ltcl8.0 -lGLU -lGL -lXmu -lX11 -lXext -lm
#LIBS = -ltk -ltcl -lMesaGLU -lMesaGL -lX11 -lXext -lm


#### Shouldn't have to change anything beyond this point


CFLAGS = $(COPTS) $(INCLUDES) $(TK_FLAGS)

LFLAGS = $(LIBDIRS)


DEMOS = double texture index


default: $(DEMOS)


double: double.o $(TOGL_DIR)/togl.o
	$(CC) $(LFLAGS) double.o $(TOGL_DIR)/togl.o $(LIBS) -o $@

double.o: double.c $(TOGL_DIR)/togl.h
	$(CC) $(CFLAGS) double.c


texture: texture.o image.o $(TOGL_DIR)/togl.o
	$(CC) $(LFLAGS) texture.o image.o $(TOGL_DIR)/togl.o $(LIBS) -o $@

texture.o: texture.c $(TOGL_DIR)/togl.h
	$(CC) $(CFLAGS) texture.c

image.o: image.c
	$(CC) $(CFLAGS) image.c



index: index.o $(TOGL_DIR)/togl.o
	$(CC) $(LFLAGS) index.o $(TOGL_DIR)/togl.o $(LIBS) -o $@

index.o: index.c $(TOGL_DIR)/togl.h
	$(CC) $(CFLAGS) index.c


$(TOGL_DIR)/togl.o: $(TOGL_DIR)/togl.c $(TOGL_DIR)/togl.h
	$(CC) -o $(TOGL_DIR)/togl.o $(CFLAGS) $(TOGL_DIR)/togl.c




clean:
	-rm *.o *~ core

realclean:
	-rm *.o *~ core
	-rm $(DEMOS)


