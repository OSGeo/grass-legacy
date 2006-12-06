MODULE_TOPDIR = ..

default: htmldocs-single

#check for program
checkdoxygen:
	@(type doxygen > /dev/null || (echo "ERROR: Install 'doxygen' software first (get from http://www.doxygen.org)" && exit 1))

# generate docs as single HTML document:
htmldocs-single: checkdoxygen clean
	doxygen $(MODULE_TOPDIR)/include/Make/Doxyfile_arch_html
	@echo "HTML reference in directory ./html/index.html"

clean:
	rm -rf ./latex ./html

#doxygen:
DOXNAME=
DOXINPUT=rfc_list.dox
