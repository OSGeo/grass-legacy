# common dependencies and rules for building shared libraries
include $(MODULE_TOPDIR)/include/Make/Platform.make

SHLIB = $(ARCH_LIBDIR)/$(SHLIB_PREFIX)$(SHLIB_NAME).$(SHLIB_SUFFIX)

#optional i18N support
ifdef HAVE_NLS
PACKAGE ="grasslibs"
DEFS=-DPACKAGE=\"$(PACKAGE)\"
NLS_CFLAGS=$(GETHOSTNAME) $(ZLIBINCPATH) $(PICFLAGS) $(DEFS)
endif

CFLAGS += $(SHLIB_CFLAGS)
LDFLAGS += $(SHLIB_LDFLAGS)

$(SHLIB): $(SHLIB_OBJS)
	$(CC) $(LDFLAGS) $(NLS_CFLAGS) $(LD_SEARCH_FLAGS) $(SHLIB_LD) $(EXTRA_LIBDIRS) $(LIBDIRS) $(TOOLS_LIBDIRS) \
		$(SHLIB_OBJS) $(EXTRA_LIBS) -o $@

shlib: $(SHLIB)


