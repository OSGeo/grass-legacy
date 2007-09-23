#NOTE: parts of the header are generated in ../../lib/gis/parser.c

# generic html rules for all commands

ifdef CROSS_COMPILING

htmlgen:

htmlcmd:

htmlcmd1:

htmlscript:

htmlscript1:

htmlinter:

htmletc:

htmletc1:

htmldir:

htmldir1:

htmlmulti:

else

htmlgen:
	@$(MODULE_TOPDIR)/tools/mkhtml.sh $(PGM)
	-$(MKDIR) $(ARCH_DISTDIR)/docs/html
	-mv -f $(PGM).tmp.html $(ARCH_DISTDIR)/docs/html/$(PGM).html
	-for file in  *.png *.jpg ; do \
		head -n 1 $$file | grep '^#!' > /dev/null ; \
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
htmlcmd: htmlcmd1 htmlgen

htmlcmd1:
	$(call htmldesc,$(BIN)/$(PGM))

# html rules for scripts
htmlscript: htmlscript1 htmlgen

htmlscript1:
	$(call htmldesc,$(ARCH_DISTDIR)/scripts/$(PGM))

# html rules for inter commands
# note that fakestart doesn't work here
htmlinter: htmlgen

# html rules for ETC commands
htmletc: htmletc1 htmlgen

htmletc1:
	$(call htmldesc,$(ETC)/$(PGM))

# html rules for intro pages in directories
htmldir: htmlgen

htmldir1:
	$(call htmldesc,$(PGM))


# html rules for multiple commands
htmlmulti:
	for prog in $(PROGRAMS) ; do $(MAKE) htmlcmd PGM=$$prog ; done

endif
