# "@(#)Makefile	AMG v.3.4";
# general mapgen support library

OPT = -O

CFLAGS = $(OPT) -I$(GRAPH)

LIB = $(MLIB)/libgen.a

lib =	$(LIB)(emess.o) $(LIB)(getline.o) $(LIB)(words.o) \
	$(LIB)(setform.o) $(LIB)(dmstor.o) $(LIB)(rtodms.o) \
	$(LIB)(twords.o) $(LIB)(adjlon.o)

install: $(lib)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ]; then ranlib $(LIB); fi

