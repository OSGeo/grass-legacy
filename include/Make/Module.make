
# common dependencies and rules for building module

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make

ifndef MOD_OBJS
MOD_OBJS = $(subst .c,.o,$(wildcard *.c))
endif

ifndef CMD_OBJS
CMD_OBJS = $(MOD_OBJS)
endif

ARCH_CMD_OBJS := $(foreach obj,$(CMD_OBJS),OBJ.$(ARCH)/$(obj))

include $(MODULE_TOPDIR)/include/Make/Rules.make

#for i18N support
PACKAGE ="grassmods"
DEFS=-DPACKAGE=\"$(PACKAGE)\"
NLS_CFLAGS=$(GETHOSTNAME) $(ZLIBINCPATH) $(PICFLAGS) $(DEFS)

cmd: $(BIN)/$(PGM) htmlcmd

inter: $(BIN)/$(PGM) htmlinter

$(BIN)/$(PGM): $(ARCH_CMD_OBJS) $(DEPENDENCIES) 
	$(CC) $(LDFLAGS) $(XTRA_LDFLAGS) $(EXTRA_CFLAGS) $(NLS_CFLAGS) -o $@ $(ARCH_CMD_OBJS) $(LIBES) $(MATHLIB) $(XDRLIB)

etc: $(ETC)/$(PGM) htmletc

$(ETC)/$(PGM): $(ARCH_CMD_OBJS) $(DEPENDENCIES) 
	$(CC) $(LDFLAGS) $(XTRA_LDFLAGS) $(EXTRA_CFLAGS) $(NLS_CFLAGS) -o $@ $(ARCH_CMD_OBJS) $(LIBES) $(MATHLIB) $(XDRLIB)
