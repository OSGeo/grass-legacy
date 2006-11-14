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
	@if ! grep -i '<html>' $(PGM).tmp.html > /dev/null 2>&1 ; then \
		echo > $(PGM).tmp.html ; \
	fi
	@if test -f $(PGM).html ; then \
		cat $(PGM).html >> $(PGM).tmp.html ; \
	elif test -f description.html ; then \
		cat description.html >> $(PGM).tmp.html ; \
	fi
	@if ! grep -i '<html>' $(PGM).tmp.html > /dev/null ; then \
		{ \
		echo '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' ; \
		echo '<html>' ; \
		echo '<head>' ; \
		echo '<title>GRASS GIS: $(PGM)</title>' ; \
		echo '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' ; \
		echo '<link rel="stylesheet" href="grassdocs.css" type="text/css">' ; \
		echo '</head>' ; \
		echo '<body bgcolor="white">' ; \
		echo '<img src="grass_logo.png" alt="GRASS logo"><hr align=center size=6 noshade>' ; \
		echo '<h2>NAME</h2>' ; \
		echo '<em><b>$(PGM)</b></em>' ; \
		} > $(PGM).tmp.html.header ; \
		grep -iv '</body>\|</html>' $(PGM).tmp.html >> $(PGM).tmp.html.header ; \
		mv -f $(PGM).tmp.html.header $(PGM).tmp.html ; \
	fi
# if </html> is found, suppose a complete html is provided.
# otherwise, generate module class reference:
	@if ! grep -i '</html>' $(PGM).tmp.html > /dev/null ; then \
		echo "<HR>" >> $(PGM).tmp.html ; \
		MODCLASS=`echo $(PGM) | cut -d'.' -f1` ; \
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
		fi ; \
		echo "<P><a href=\"index.html\">Main index</a> - <a href=\"$$INDEXNAME.html\">$$INDEXNAME index</a> - <a href=\"full_index.html\">Full index</a></P>" >> $(PGM).tmp.html ; \
		echo "</body>" >> $(PGM).tmp.html ; \
		echo "</html>" >> $(PGM).tmp.html ; \
	fi
	-$(MKDIR) $(ARCH_DISTDIR)/docs/html
	-mv -f $(PGM).tmp.html $(ARCH_DISTDIR)/docs/html/$(PGM).html
	-for file in  *.png *.jpg ; do \
		head -n 1 $$file | grep '^#!' > /dev/null ; \
		if [ $$? -ne 0 ] ; then \
		   $(INSTALL_DATA) $$file $(ARCH_DISTDIR)/docs/html ; \
		fi \
		done 2> /dev/null ; true

htmldesc = \
	GISRC=$(RUN_GISBASE)/demolocation/.grassrc${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR} \
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
