MODULE_TOPDIR = ../../../../..

include $(MODULE_TOPDIR)/include/Make/Module.make

TPIEINC = ../../include

LIBRARY = libiostream.a

RANLIB = ranlib

ARFLAGS = rv

# This is library code.
override CPPFLAGS += -I$(TPIEINC) -g -D_FILE_OFFSET_BITS=64

SRC = mm.cc mm_utils.cc ami_stream.cc rtimer.cc

OBJ = $(SRC:.cc=.o)

LIB_COMPONENTS = $(patsubst %.cc,$(LIBRARY)(%.o),$(SRC))

(%): %
	$(AR) $(ARFLAGS) $@ $<

default: $(LIBRARY)

$(LIBRARY): $(LIB_COMPONENTS)


clean:
	-rm $(LIBRARY)
