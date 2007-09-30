#NOTE: parts of the header are generated in ../../lib/gis/parser.c

HTMLDIR = $(ARCH_DISTDIR)/docs/html

# generic html rules for all commands

ifdef CROSS_COMPILING

html:

htmlcmd:

htmlscript:

htmlinter:

htmletc:

htmldir:

htmlmulti:

else

htmlgen = \
	$(MODULE_TOPDIR)/tools/mkhtml.sh $(PGM) ; \
	$(MKDIR) $(HTMLDIR) ; \
	$(INSTALL_DATA) $(PGM).tmp.html $(HTMLDIR)/$(PGM).html ; \
	for file in  *.png *.jpg ; do \
		head -n 1 $$file | grep '^\#!' > /dev/null ; \
		if [ $$? -ne 0 ] ; then \
		   $(INSTALL_DATA) $$file $(HTMLDIR) ; \
		fi \
		done 2> /dev/null ; true

htmldesc = \
	GISRC=$(RUN_GISRC) \
	GISBASE=$(RUN_GISBASE) \
	PATH="$(BIN):$$PATH" \
	$(LD_LIBRARY_PATH_VAR)="$(BIN):$(ARCH_LIBDIR):$($(LD_LIBRARY_PATH_VAR))" \
	LC_ALL=C $(1) --html-description < /dev/null | grep -v '</body>\|</html>' > $(PGM).tmp.html ; true

%.tmp.html: $(BIN)/%$(EXE)
	$(call htmldesc,$<)

%.tmp.html: $(ETC)/%$(EXE)
	$(call htmldesc,$<)

%.tmp.html: $(SCRIPTDIR)/%
	$(call htmldesc,$<)

%.tmp.html: %.html
	$(INSTALL_DATA) $< $@

$(HTMLDIR)/%.html: %.tmp.html
	$(call htmlgen)

$(HTMLDIR)/.html:


html: $(HTMLDIR)/$(PGM).html

# html rules for cmd commands
htmlcmd: html

# html rules for scripts
htmlscript: html

# html rules for inter commands
# note that fakestart doesn't work here
htmlinter: html

# html rules for ETC commands
# does not have "html" as a dependency so that it can be overridden in Makefiles
htmletc:
	$(MAKE) html

# html rules for intro pages in directories
htmldir: html

# html rules for multiple commands
htmlmulti:
	for prog in $(PROGRAMS) ; do $(MAKE) htmlcmd PGM=$$prog ; done

endif
