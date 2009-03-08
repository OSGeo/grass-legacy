# common dependencies and rules for building shared libraries

SHLIB = $(ARCH_LIBDIR)/$(SHLIB_PREFIX)$(SHLIB_NAME).$(GRASS_VERSION_NUMBER)$(SHLIB_SUFFIX)

# Object that calls _setfmode(_O_BINARY) which must be linked to each DLL on Windows
ifdef MINGW
ifneq ($(SHLIB_NAME),$(DATETIME_LIBNAME))
ifneq ($(SHLIB_NAME),$(GIS_LIBNAME))
  DLLMAIN_OBJ = $(MODULE_TOPDIR)/lib/gis/$(OBJDIR)/dllmain.o
endif
endif
endif

CFLAGS += $(SHLIB_CFLAGS) $(NLS_CFLAGS)
LDFLAGS += $(SHLIB_LDFLAGS)

$(SHLIB): $(SHLIB_OBJS)
	$(SHLIB_LD) -o $@ $(LDFLAGS) $^ $(EXTRA_LIBS) $(DLLMAIN_OBJ) && \
	(cd $(ARCH_LIBDIR); ln -f -s $(notdir $@) $(patsubst %.$(GRASS_VERSION_NUMBER)$(SHLIB_SUFFIX),%$(SHLIB_SUFFIX),$@))

shlib: $(SHLIB)

