#NOTE: parts of the header are generated in ../../lib/gis/parser.c

# generic html rules for all commands

ifdef CROSS_COMPILING

htmlcmd:

htmlscript:

htmlinter:

htmletc:

htmldir:

htmlmulti:

else

htmlgen = \
	$(MODULE_TOPDIR)/tools/mkhtml.sh $(PGM) ; \
	$(MKDIR) $(ARCH_DISTDIR)/docs/html ; \
	mv -f $(PGM).tmp.html $(ARCH_DISTDIR)/docs/html/$(PGM).html ; \
	for file in  *.png *.jpg ; do \
		head -n 1 $$file | grep '^\#!' > /dev/null ; \
		if [ $$? -ne 0 ] ; then \
		   $(INSTALL_DATA) $$file $(ARCH_DISTDIR)/docs/html ; \
		fi \
		done 2> /dev/null ; true

htmldesc = \
	GISRC=$(RUN_GISRC) \
	GISBASE=$(RUN_GISBASE) \
	PATH="$(BIN):$$PATH" \
	$(LD_LIBRARY_PATH_VAR)="$(BIN):$(ARCH_LIBDIR):$($(LD_LIBRARY_PATH_VAR))" \
	LC_ALL=C $(1) --html-description | grep -v '</body>\|</html>' > $(PGM).tmp.html ; true

# html rules for cmd commands
htmlcmd: $(PROG)
	$(call htmldesc,$(BIN)/$(PGM))
	$(call htmlgen)

# html rules for scripts
htmlscript: $(PROGDIR)/$(PGM)
	$(call htmldesc,$(ARCH_DISTDIR)/scripts/$(PGM))
	$(call htmlgen)

# html rules for inter commands
# note that fakestart doesn't work here
htmlinter: $(PROG)
	$(call htmlgen)

# html rules for ETC commands
htmletc: $(ETC)/$(PGM)$(EXE)
	$(call htmldesc,$(ETC)/$(PGM))
	$(call htmlgen)

# html rules for intro pages in directories
htmldir:
	$(call htmldesc,$(PGM))
	$(call htmlgen)

# html rules for multiple commands
htmlmulti:
	for prog in $(PROGRAMS) ; do $(MAKE) htmlcmd PGM=$$prog ; done

endif
