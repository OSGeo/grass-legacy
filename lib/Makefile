MODULE_TOPDIR = ..

#order is relevant:
SUBDIRS = \
	datetime \
	gis \
	gmath \
	linkm \
	raster \
	bitmap \
	btree \
	display \
	D \
	db \
	vask \
	edit \
	external \
	fonts \
	gtcltk \
	form \
	image \
	imagery \
	proj \
	rowio \
	segment \
	vector \
	sites \
	rst \
	g3d \
	dspf \
	symbol \
	init \
	cdhc

include $(MODULE_TOPDIR)/include/Make/Dir.make

OPENGLBASED = ogsf

#compile if OPENGLBASED present:
ifneq ($(strip $(OPENGLLIB)),)
    SUBDIRS += $(OPENGLBASED)
endif

default: headers subdirs

headers: 
	if [ ! -d $(GISBASE)/include/grass ]; then $(MKDIR) $(GISBASE)/include/grass; fi
	if [ ! -d $(GISBASE)/include/grass/vect ]; then $(MKDIR) $(GISBASE)/include/grass/vect; fi
	$(INSTALL) -m 644 ../include/*.h $(GISBASE)/include/grass/
	$(INSTALL) -m 644 ../include/vect/*.h $(GISBASE)/include/grass/vect/

clean: cleansubdirs

DOXNAME=grasslibs
#check for program
# also needs 'dot' tool from Graphviz (http://www.research.att.com/sw/tools/graphviz/)
checkdoxygen:
	@(type doxygen > /dev/null || (echo "ERROR: Install 'doxygen' software first (get from http://www.doxygen.org)" && exit 1))

#generates FULL html documentation
htmldocs: checkdoxygen
	doxygen ./gis/Doxyfile_arch_html
	@rm -f ./html/*.md5
	@echo "HTML reference in directory ./html/index.html"

latexdocs: checkdoxygen
	test ! -d latex || (cd ./latex && $(MAKE) clean)
	doxygen ./gis/Doxyfile_arch_latex
#this hack is needed to make Acroread's search engine happy:
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) )
	@rm -f ./latex/*.md5
	@echo "Latex reference in directory ./latex/refman.dvi"

#may leet to TeX capacity exceeded - see doxygen FAQ at www.doxygen.org
pdfdocs: checkdoxygen
	test ! -d latex || (cd ./latex && $(MAKE) clean)
	doxygen ./gis/Doxyfile_arch_latex
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) refman.pdf && mv refman.pdf grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}${DOXNAME}_`date '+%Y_%m_%d'`_refman.pdf)
	@rm -f ./latex/*.md5
	@echo "PDF reference in directory ./latex/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}${DOXNAME}_`date '+%Y_%m_%d'`_refman.pdf"

cleandocs:
	rm -rf ./latex ./html
