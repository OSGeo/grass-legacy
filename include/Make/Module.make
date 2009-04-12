
# common dependencies and rules for building module

PACKAGE ="grassmods"

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

cmd: $(BIN)/$(PGM)$(EXE)
	$(MAKE) htmlcmd
	$(MAKE) mancmd

$(BIN)/$(PGM)$(EXE): $(ARCH_CMD_OBJS) $(DEPENDENCIES) 
	$(CC) $(LDFLAGS) $(XTRA_LDFLAGS) -o $@ $(ARCH_CMD_OBJS) $(FMODE_OBJ) $(LIBES) $(MATHLIB) $(XDRLIB)

etc: $(ETC)/$(PGM)$(EXE)
	$(MAKE)  htmletc
	$(MAKE)  manetc

$(ETC)/$(PGM)$(EXE): $(ARCH_CMD_OBJS) $(DEPENDENCIES) 
	$(CC) $(LDFLAGS) $(XTRA_LDFLAGS) -o $@ $(ARCH_CMD_OBJS) $(FMODE_OBJ) $(LIBES) $(MATHLIB) $(XDRLIB)

install:
	$(INSTALL) $(ARCH_DISTDIR)/bin/$(PGM)$(EXE) $(INST_DIR)/bin/
	$(INSTALL_DATA) $(HTMLDIR)/$(PGM).html $(INST_DIR)/docs/html/
	$(INSTALL_DATA) $(ARCH_DISTDIR)/man/man1/$(PGM).1 $(INST_DIR)/man/man1/

.PHONY: cmd etc install
