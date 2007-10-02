# lexical analyzer and default options
LEXFLAGS= -d -i -s -t

# parser generator and default options
YACCFLAGS = -d -v -t

ifndef LOCAL_HEADERS
LOCAL_HEADERS = $(wildcard *.h)
endif

# for i18N support
PACKAGE ="grassmods"
DEFS=-DPACKAGE=\"$(PACKAGE)\"
NLS_CFLAGS=$(GETHOSTNAME) $(ZLIBINCPATH) $(PICFLAGS) $(DEFS)

$(OBJDIR):
	-test -d $(OBJDIR) || $(MKDIR) $(OBJDIR)

# default cc rules
$(OBJDIR)/%.o : %.c $(LOCAL_HEADERS) | $(OBJDIR)
	$(CC) $(CFLAGS) $(EXTRA_CFLAGS) $(NLS_CFLAGS) $(EXTRA_INC) $(INC) \
		-o $(OBJDIR)/$*.o -c $*.c

# default parser generation rules, include prefix for files/vars
%.yy.c: %.l
	$(LEX) -P$* $(LEXFLAGS) $*.l | \
	$(SED) -e 's/unistd.h/limits.h/g' \
	> $@

%.tab.h %.tab.c: %.y
	$(YACC) -b$* -p$* $(YACCFLAGS) $<


# default clean rules
clean:
	-rm -rf $(OBJDIR) $(EXTRA_CLEAN_DIRS)
	-rm -f $(EXTRA_CLEAN_FILES) *.tmp.html
	-if [ "$(CLEAN_SUBDIRS)" != "" ] ; then \
		for dir in $(CLEAN_SUBDIRS) ; do \
			$(MAKE) -C $$dir clean ; \
		done ; \
	fi

# HTML page rules:
include $(MODULE_TOPDIR)/include/Make/Html.make
