# common dependencies and rules for building shared libraries

SHLIB = $(ARCH_LIBDIR)/$(SHLIB_PREFIX)$(SHLIB_NAME).$(VERSION_NUMBER)$(SHLIB_SUFFIX)

#for i18N support
PACKAGE ="grasslibs"
DEFS=-DPACKAGE=\"$(PACKAGE)\"
NLS_CFLAGS=$(GETHOSTNAME) $(ZLIBINCPATH) $(PICFLAGS) $(DEFS)

CFLAGS += $(SHLIB_CFLAGS) $(NLS_CFLAGS)
LDFLAGS += $(SHLIB_LDFLAGS)

$(SHLIB): $(SHLIB_OBJS)
	$(SHLIB_LD) -o $@ $(LDFLAGS) $^ $(EXTRA_LIBS) && ln -sf $(notdir $@) $(patsubst %.$(VERSION_NUMBER)$(SHLIB_SUFFIX),%$(SHLIB_SUFFIX),$@)

shlib: $(SHLIB)

