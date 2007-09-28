LIBRARY = libiostream.a

LIBSRCPATH = ./src

RANLIB = ranlib

export

all: $(LIBRARY)


$(LIBSRCPATH)/$(LIBRARY):
	$(MAKE) -C $(LIBSRCPATH)

$(LIBRARY): $(LIBSRCPATH)/$(LIBRARY)
	cp $< $@
	$(RANLIB) $@

clean:
	-rm $(LIBRARY)
	-rm $(LIBSRCPATH)/$(LIBRARY)
	-rm $(LIBSRCPATH)/*.o
