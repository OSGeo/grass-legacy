# "@(#)Makefile	AMG v.3.2";
# general support library

OPT = -O

CFLAGS = $(OPT) -I$(GRAPH)/src

LIB = $(GRAPH)/src/lib/libgen.a

lib =	$(LIB)(emess.o) $(LIB)(getline.o) $(LIB)(words.o) \
	$(LIB)(setform.o) $(LIB)(dmstor.o) $(LIB)(rtodms.o) \
	$(LIB)(twords.o) $(LIB)(adjlon.o)

install: $(lib)
	$(RANLIB) $(LIB)

