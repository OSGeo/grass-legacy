MODULE_TOPDIR = ..

DOXNAME=grassswig
#check for program
# also needs 'dot' tool from Graphviz (http://www.research.att.com/sw/tools/graphviz/)
checkdoxygen:
	@(type doxygen > /dev/null || (echo "ERROR: Install 'doxygen' software first (get from http://www.doxygen.org)" && exit 1))

#generates FULL html documentation
htmldocs: checkdoxygen
	doxygen ../lib/gis/Doxyfile_arch_html
	@rm -f ./html/*.md5
	@echo "HTML reference in directory ./html/index.html"

latexdocs: checkdoxygen
	test ! -d latex || (cd ./latex && $(MAKE) clean)
	doxygen ../lib/gis/Doxyfile_arch_latex
#this hack is needed to make Acroread's search engine happy:
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) )
	@rm -f ./latex/*.md5
	@echo "Latex reference in directory ./latex/refman.dvi"

#may leet to TeX capacity exceeded - see doxygen FAQ at www.doxygen.org
pdfdocs: checkdoxygen
	test ! -d latex || (cd ./latex && $(MAKE) clean)
	doxygen ../lib/gis/Doxyfile_arch_latex
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) refman.pdf && mv refman.pdf grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}${DOXNAME}_`date '+%Y_%m_%d'`_refman.pdf)
	@rm -f ./latex/*.md5
	@echo "PDF reference in directory ./latex/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}${DOXNAME}_`date '+%Y_%m_%d'`_refman.pdf"

cleandocs:
	rm -rf ./latex ./html
