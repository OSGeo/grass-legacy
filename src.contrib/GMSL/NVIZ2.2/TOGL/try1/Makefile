# Makefile for Togl demos (version 1.0)


# The C compiler:
CC = cc

# Compiler options:
COPTS = -c -O -cckr

TOGL_DIR = ..


# Where to find tcl.h, tk.h, OpenGL/Mesa headers, etc:
INCLUDES = -I/usr/include -I/usr/include/X11 -I/usr/local/encap/tcl7.4/include -I/usr/local/encap/tk4.0/include -I$(TOGL_DIR)
#INCLUDES = -I/usr/local/include
#INCLUDES = -I/usr2/people/brianp/Mesa/include -I/usr/local/include

# Where to find libtcl.a, libtk.a, OpenGL/Mesa libraries:
LIBDIRS = -L/usr/local/lib
#LIBDIRS = -L/usr/local/lib -L/usr2/people/brianp/Mesa/lib 

# Libraries to link with:
LIBS = -ltk4.0 -ltcl7.4 -lGLU -lGL -lX11 -lXmu -lXext -lm
#LIBS = -ltk -ltcl -lMesaGLU -lMesaGL -lX11 -lXext -lm


#### Shouldn't have to change anything beyond this point


CFLAGS = $(COPTS) $(INCLUDES) $(TK_FLAGS)

LFLAGS = $(LIBDIRS)


DEMOS = mydouble


default: $(DEMOS)


mydouble: double.o $(TOGL_DIR)/togl.o
	$(CC) $(LFLAGS) double.o $(TOGL_DIR)/togl.o $(LIBS) -o $@

double.o: double.c $(TOGL_DIR)/togl.h
	$(CC) $(CFLAGS) double.c

$(TOGL_DIR)/togl.o: $(TOGL_DIR)/togl.c $(TOGL_DIR)/togl.h
	$(CC) -o $(TOGL_DIR)/togl.o $(CFLAGS) $(TOGL_DIR)/togl.c


clean:
	-rm *.o *~ core

realclean:
	-rm *.o *~ core
	-rm $(DEMOS)


