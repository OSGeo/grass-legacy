LIBRARY = libiostream.a

LIBSRCPATH = ./src

RANLIB = ranlib

export

all: build $(LIBRARY)


build:
	(cd src ; make)

$(LIBRARY): $(LIBSRCPATH)/$(LIBRARY)
	cp $< $@
	$(RANLIB) $@

clean:
	-rm $(LIBRARY)
#	$(MAKE) -C $(LIBSRCPATH) $@
	-rm $(LIBSRCPATH)/*.d $(LIBSRCPATH)/*.o
