
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
	#generate module class reference:
	@MODCLASS=`echo $(PGM) | cut -d'.' -f1` ; \
	if [ "$$MODCLASS" = "d" ]  ; then INDEXNAME=`echo display` ; \
	elif [ $$MODCLASS = "db" ] ; then INDEXNAME=`echo database` ; \
	elif [ $$MODCLASS = "g" ]  ; then INDEXNAME=`echo general` ; \
	elif [ $$MODCLASS = "i" ]  ; then INDEXNAME=`echo imagery` ; \
	elif [ $$MODCLASS = "m" ]  ; then INDEXNAME=`echo misc` ; \
	elif [ $$MODCLASS = "pg" ] ; then INDEXNAME=`echo postGRASS` ; \
	elif [ $$MODCLASS = "ps" ] ; then INDEXNAME=`echo postscript` ; \
	elif [ $$MODCLASS = "p" ]  ; then INDEXNAME=`echo paint` ; \
	elif [ $$MODCLASS = "r" ]  ; then INDEXNAME=`echo raster` ; \
	elif [ $$MODCLASS = "r3" ] ; then INDEXNAME=`echo raster3D` ; \
	elif [ $$MODCLASS = "s" ]  ; then INDEXNAME=`echo sites` ; \
	elif [ $$MODCLASS = "v" ]  ; then INDEXNAME=`echo vector` ; \
	else \
	    INDEXNAME=`echo $$MODCLASS` ; \
	fi ; echo "<P><a href=\"index.html\">Main index</a> - <a href=\"$$INDEXNAME.html\">$$INDEXNAME index</a> - <a href=\"full_index.html\">Full index</a>" >> $(PGM).tmp.html
	echo "</body></html>" >> $(PGM).tmp.html
	-$(MKDIR) $(GISBASE)/docs/html
	-mv -f $(PGM).tmp.html $(GISBASE)/docs/html/$(PGM).html
	-$(INSTALL) *.png *.jpg $(GISBASE)/docs/html 2> /dev/null ; true

htmldesc = \
	GISRC=$(GISBASE)/demolocation/.grassrc${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR} \
	GISBASE=$(GISBASE) \
	PATH=$(GISBASE)/bin:$$PATH \
	$(LD_LIBRARY_PATH_VAR)="$(GISBASE)/lib:$($(LD_LIBRARY_PATH_VAR))" \
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
