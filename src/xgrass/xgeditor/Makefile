CC=cc
RM=rm -f
CFLAGS=-I/grass.src/xgrass/src/lib -I. -I/grass.src/xgrass/src/install/include -D_NO_PROTO -g
LIBS = -L/grass.src/xgrass/src/install/lib -lXg -lXm -lXt -lX11

SRC = editor.c
OBJ = editor.o

all:: $(OBJ)
	cc -o editor $(OBJ) $(LIBS)


