# common dependencies and rules for building shared libraries
include $(MODULE_TOPDIR)/include/Make/Platform.make

SHLIB = $(ARCH_LIBDIR)/$(SHLIB_PREFIX)$(SHLIB_NAME)$(SHLIB_SUFFIX)

#for i18N support
PACKAGE ="grasslibs"
DEFS=-DPACKAGE=\"$(PACKAGE)\"
NLS_CFLAGS=$(GETHOSTNAME) $(ZLIBINCPATH) $(PICFLAGS) $(DEFS)

CFLAGS += $(SHLIB_CFLAGS) $(NLS_CFLAGS)
LDFLAGS += $(SHLIB_LDFLAGS)

$(SHLIB): $(SHLIB_OBJS)
	$(SHLIB_LD) -o $@ $(LDFLAGS) $^ $(EXTRA_LIBS)

shlib: $(SHLIB)

