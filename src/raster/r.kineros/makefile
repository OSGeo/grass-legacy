PGM    = r.kineros
HOME   = .
SRC    = /geo/grass4.1/src
LIBDIR = $(SRC)/libes/LIB
GISLIB = $(LIBDIR)/libgis.a
HFILES = global.h define.h
LIBS   = -lm

OFILES =	contrib_planes.o \
		debug.o \
		find_stream.o \
		find_tribs.o \
		fmatrix.o \
		free_fmatrix.o \
		free_fvector.o \
		free_imatrix.o \
		free_ivector.o \
		fvector.o \
		imatrix.o \
		index.o \
		initialize.o \
		ivector.o \
		main.o \
		open_file.o \
		plane_geom.o \
		print_ele.o \
		read_files.o \
		read_float.o \
		read_int.o \
		read_par.o \
		soil_param.o \
		stream_order.o \
		stream_width.o \
		tribs.o 

$(HOME)/$(PGM): $(OFILES) $(GISLIB)
	$(CC) $(OFILES) $(GISLIB) $(LIBS) -o $(PGM)
$(OFILES):	$(HFILES)
$(GISLIB): #
