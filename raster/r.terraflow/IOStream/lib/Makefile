LIBRARY = libiostream.a

LIBSRCPATH = ./src

RANLIB = ranlib

export

all: build $(LIBRARY)


build:
	(cd src ; $(MAKE) )

$(LIBRARY): $(LIBSRCPATH)/$(LIBRARY)
	cp $< $@
	$(RANLIB) $@

clean:
	-rm $(LIBRARY)
	-rm $(LIBSRCPATH)/$(LIBRARY)
#	$(MAKE) -C $(LIBSRCPATH) $@
	-rm $(LIBSRCPATH)/*.d $(LIBSRCPATH)/*.o
