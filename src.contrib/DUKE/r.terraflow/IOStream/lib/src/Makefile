TPIEINC = ../../include

LIBRARY = libiostream.a

RANLIB = ranlib

ARFLAGS = rv

CXX = g++ -Wall

# This is library code.
override CPPFLAGS += -I$(TPIEINC) -g

SRC = mm.cc mm_utils.cc ami_stream.cc rtimer.cc

OBJ = $(SRC:.cc=.o)

LIB_COMPONENTS = $(patsubst %.cc,$(LIBRARY)(%.o),$(SRC))

(%): %
	$(AR) $(ARFLAGS) $@ $<

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< | sed '\''s/$*.o/& $@/g'\'' > $@'

all : $(LIBRARY)

# Don't assume that source directory is writable
# include $(SRC:.cc=.d)

$(LIBRARY): $(LIB_COMPONENTS)


clean:
	-rm $(SRC:.cc=.d) $(OBJ)
	-rm $(LIBRARY)
