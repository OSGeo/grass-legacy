# auto-generate typedef file

all:
	echo "/* auto-generate swig typedef file (with some GRASS functions removed) */" > gisdefs.i
	cat ../../../include/gisdefs.h | grep -v '#\|asprintf\|G_fatal_error\|G_warning\|lzw' >> gisdefs.i
