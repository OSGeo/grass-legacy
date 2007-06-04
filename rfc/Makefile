MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Doxygen.make

all:
	doxygen $(MODULE_TOPDIR)/include/Make/Doxyfile_arch_html
	@echo ""
	@echo "RFC list generated: html/index.html"

htmldocs-single: all

clean:
	rm -fr latex/ html/
cleandocs: clean

DOXNAME=rfc_list
DOXINPUT=$(DOXNAME).dox
DOXOUTPUT=$(DOXNAME)

pdfdocs:
	@echo "disabled for now due to Latex error (why fails?)"
