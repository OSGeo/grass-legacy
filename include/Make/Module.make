
# common dependencies and rules for building module

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

ifndef MOD_OBJS
MOD_OBJS = $(subst .c,.o,$(wildcard *.c))
endif

ifndef CMD_OBJS
CMD_OBJS = $(MOD_OBJS)
endif

ifndef INTER_OBJS
INTER_OBJS = $(MOD_OBJS)
endif

ARCH_CMD_OBJS := $(foreach obj,$(CMD_OBJS),OBJ.$(ARCH)/$(obj))
ARCH_INTER_OBJS := $(foreach obj,$(INTER_OBJS),OBJ.$(ARCH)/$(obj))

include $(MODULE_TOPDIR)/include/Make/Rules.make

cmd: $(BIN_CMD)/$(PGM)

$(BIN_CMD)/$(PGM): $(ARCH_CMD_OBJS) $(DEPENDENCIES) 
	$(CC) $(LDFLAGS) $(XTRA_LDFLAGS) -o $@ $(ARCH_CMD_OBJS) $(LIBES) $(MATHLIB) $(XDRLIB)
	@test -e $(BIN)/$(PGM) || ln $(ETC)/front.end $(BIN)/$(PGM)
