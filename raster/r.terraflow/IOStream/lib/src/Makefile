MODULE_TOPDIR = ../../../../..

include $(MODULE_TOPDIR)/include/Make/Module.make

TPIEINC = ../../include

LIBRARY = libiostream.a

RANLIB = ranlib

ARFLAGS = rv

# This is library code.
override CPPFLAGS += -I$(TPIEINC) -g

SRC = mm.cc mm_utils.cc ami_stream.cc rtimer.cc

OBJ = $(SRC:.cc=.o)

LIB_COMPONENTS = $(patsubst %.cc,$(LIBRARY)(%.o),$(SRC))

(%): %
	$(AR) $(ARFLAGS) $@ $<

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< | sed '\''s/$*.o/& $@/g'\'' > $@'

default: $(LIBRARY)

include $(SRC:.cc=.d)

$(LIBRARY): $(LIB_COMPONENTS)


clean:
	-rm $(SRC:.cc=.d) $(OBJ)
	-rm $(LIBRARY)
