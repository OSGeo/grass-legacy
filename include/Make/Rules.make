
# lexical analyzer and default options
LEXFLAGS= -d -i -s -t

# parser generator and default options
YACCFLAGS = -d -v -t

ifndef LOCAL_HEADERS
LOCAL_HEADERS = $(wildcard *.h)
endif

#for i18N support
PACKAGE ="grassmods"
DEFS=-DPACKAGE=\"$(PACKAGE)\"
NLS_CFLAGS=$(GETHOSTNAME) $(ZLIBINCPATH) $(PICFLAGS) $(DEFS)

# default cc rules
$(OBJDIR)/%.o : %.c $(DEPENDENCIES) $(LOCAL_HEADERS) 
	@test -d $(OBJDIR) || mkdir $(OBJDIR)	
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
	-rm -rf $(OBJDIR) $(EXTRA_CLEAN_DIRS) $(EXTRA_CLEAN_FILES)

# generic html rules for all commands
htmlgen:
	@if test -f $(PGM).html ; then \
		cat $(PGM).html >> $(PGM).tmp.html ; \
	elif test -f description.html ; then \
		cat description.html >> $(PGM).tmp.html ; \
	fi
	echo "<HR>" >> $(PGM).tmp.html
	echo "<P><a href=index.html>Help Index</a>" >> $(PGM).tmp.html
	echo "</body></html>" >> $(PGM).tmp.html
	-$(MKDIR) $(GISBASE)/docs/html
	-mv -f $(PGM).tmp.html $(GISBASE)/docs/html/$(PGM).html
	-$(INSTALL) *.png *.jpg $(GISBASE)/docs/html 2> /dev/null ; true

htmldesc = \
	GISRC=$(GISBASE)/demolocation/.grassrc${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR} \
	GISBASE=$(GISBASE) \
	PATH=$(GISBASE)/bin:$$PATH \
	$(LD_LIBRARY_PATH_VAR)="$($(LD_LIBRARY_PATH_VAR)):$(GISBASE)/lib" \
	$(1) --html-description | grep -v '</body>' > $(PGM).tmp.html ; true

# html rules for cmd commands
htmlcmd: htmlcmd1 htmlgen

htmlcmd1:
	$(call htmldesc,$(BIN)/$(PGM))

# html rules for scripts
htmlscript: htmlscript1 htmlgen

htmlscript1:
	$(call htmldesc,$(GISBASE)/scripts/$(PGM))

# html rules for inter commands
# note that fakestart doesn't work here
htmlinter: htmlgen

# html rules for ETC commands
htmletc: htmletc1 htmlgen

htmletc1:
	$(call htmldesc,$(ETC)/$(PGM))

# html rules for multiple commands
htmlmulti:
	for prog in $(PROGRAMS) ; do $(MAKE) htmlcmd PGM=$$prog ; done
