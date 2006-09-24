# auto-generate typedef file

# Note: gis contains the raster data API
all: gis vector

gis:
	echo "/* auto-generate swig typedef file (with some GRASS functions removed) */" > gisdefs.i
	cat ../../../include/gisdefs.h | grep -v '#\|asprintf\|G_fatal_error\|G_warning\|G_verbose\|lzw' >> gisdefs.i

vector:
	echo "/* auto-generate swig typedef file */" > vect.i
	cat ../../../include/Vect.h | grep -v 'V1_\|V2_' >> vect.i
