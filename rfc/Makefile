MODULE_TOPDIR = ..

all:
	doxygen $(MODULE_TOPDIR)/include/Make/Doxyfile_arch_html
	@echo ""
	@echo "RFC list generated: html/index.html"

htmldocs-single: all

clean:
	rm -fr latex/ html/
cleandocs: clean

htmldocs:

pdfdocs:
