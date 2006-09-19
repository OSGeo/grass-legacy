# auto-generate typedef file

all:
	echo "/* auto-generate swig typedef file (with some GRASS functions removed) */" > gisdefs.i
	cat ../../../include/gisdefs.h | grep -v '#\|asprintf\|G_def_window\|G_edit\|G_message\|G_fatal_error\|G_warning\|lzw\|open_support' >> gisdefs.i
