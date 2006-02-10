# common dependencies and rules for building shared libraries

ifdef MINGW
SHLIB = $(ARCH_LIBDIR)/$(SHLIB_PREFIX)$(SHLIB_NAME)$(SHLIB_SUFFIX)
else
SHLIB = $(ARCH_LIBDIR)/$(SHLIB_PREFIX)$(SHLIB_NAME).$(GRASS_VERSION_NUMBER)$(SHLIB_SUFFIX)
endif

#for i18N support
PACKAGE ="grasslibs"
DEFS=-DPACKAGE=\"$(PACKAGE)\"
NLS_CFLAGS=$(GETHOSTNAME) $(ZLIBINCPATH) $(PICFLAGS) $(DEFS)

CFLAGS += $(SHLIB_CFLAGS) $(NLS_CFLAGS)
LDFLAGS += $(SHLIB_LDFLAGS)

$(SHLIB): $(SHLIB_OBJS)
	$(SHLIB_LD) -o $@ $(LDFLAGS) $^ $(EXTRA_LIBS) && if [ -z "${MINGW}" ] ; then ln -f -s $(notdir $@) $(patsubst %.$(GRASS_VERSION_NUMBER)$(SHLIB_SUFFIX),%$(SHLIB_SUFFIX),$@); fi

shlib: $(SHLIB)

