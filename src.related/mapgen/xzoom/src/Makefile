
XLIB = -lXaw -lXmu -lXt -lX11 -lm

CC = cc

#CFLAGS = -g

# select which release of graphics are employed
# pre version 4
#CFLAGS = -O $(INCL) -DXVERSION=\"1.02\" -DPRE4
# version 4 or later
CFLAGS = -O -I$(INCL) -DXVERSION=\"1.02\"

OBJ = xzoom.o command.o gr.o scalein.o words.o getline.o

SRC = xzoom.c command.c gr.c scalein.c words.c getline.c

xzoom:	$(OBJ)
	cc -o xzoom $(OBJ) $(XLIB)

$(OBJ): xzoom.h
xzoom.o: xzoom.icon

lint:
	lint $(INCL) $(SRC)

clean:
	- /bin/rm *.o xzoom
