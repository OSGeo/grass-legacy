#include <unistd.h>
#include <stdlib.h>
#include "gis.h"
#include "global.h"
#include "local_proto.h"

FILE *Tmp_fd = NULL;
char *Tmp_file = NULL;

int 
main (int argc, char *argv[])
{
	char *input;
	char *output;
	char *title;
	char err[64];
	char tmp[1024];
	FILE *fd;
	int cf;
	struct Cell_head cellhd;
	void *rast, *rast_ptr;
	int row, col;
	int nrows, ncols;
	double x;
	char y[128];
	struct GModule *module;
	struct
	{
		struct Option *input, *output, *title, *mult, *nv;
	} parm;
	struct
	{
		struct Flag *i, *f, *d;
	} flag;
	char *null_val_str;
	DCELL mult;
	RASTER_MAP_TYPE data_type;
	double atof();

	G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Convert an ASCII raster text file into a (binary) raster map layer.";

	parm.input = G_define_option();
	parm.input->key = "input";
	parm.input->type = TYPE_STRING;
	parm.input->required = YES;
	parm.input->description = "Ascii raster file to be imported";

	parm.output = G_define_option();
	parm.output->key = "output";
	parm.output->type = TYPE_STRING;
	parm.output->required = YES;
	parm.output->description = "Name for resultant raster map";
	parm.output->gisprompt = "any,cell,raster";

	parm.title = G_define_option();
	parm.title->key = "title";
	parm.title->key_desc = "\"phrase\"";
	parm.title->type = TYPE_STRING;
	parm.title->required = NO;
	parm.title->description = "Title for resultant raster map";

	parm.mult = G_define_option();
	parm.mult->key = "mult";
	parm.mult->type = TYPE_DOUBLE;
	parm.mult->answer = "1.0 or read from header";
	parm.mult->required = NO;
	parm.mult->description = "Multiplier for ascii data" ;

        parm.nv = G_define_option();
        parm.nv->key = "nv";
        parm.nv->type = TYPE_STRING;
        parm.nv->required = NO;
        parm.nv->multiple = NO;
	parm.nv->answer = "* or read from header";
        parm.nv->description = "String representing NULL value data cell";

	flag.i = G_define_flag();
	flag.i->key = 'i';
	flag.i->description = 
		"integer values are imported";

	flag.f = G_define_flag();
	flag.f->key = 'f';
	flag.f->description = 
		"floating point values are imported";

	flag.d = G_define_flag();
	flag.d->key = 'd';
	flag.d->description = 
	     "double floating point values are imported";

	if (G_parser(argc,argv))
		exit(1);
	input = parm.input->answer;
	output = parm.output->answer;
	if(title = parm.title->answer)
		G_strip (title);
        if(strcmp(parm.mult->answer, "1.0 or read from header")==0)
	    G_set_d_null_value(&mult, 1);
        else
	   if ((sscanf(parm.mult->answer,"%lf",&mult)) != 1) 
	   {
	      sprintf(tmp, "wrong entry for multiplier: %s", parm.mult->answer);
	      G_usage();
	      exit(-1) ;
           }
	if(strcmp(parm.nv->answer, "* or read from header")==0)
	   null_val_str = NULL;
        else
	   null_val_str = parm.nv->answer;

        data_type = -1;
	if (flag.i->answer) {		/* interger data */
	  data_type = CELL_TYPE;
	}
	if (flag.f->answer) {		/* floating-point data */
	  data_type = FCELL_TYPE;
	}
	if (flag.d->answer) {		/* double data; overwrite others */
	  data_type = DCELL_TYPE;
	} 

	if (strcmp ("-", input) == 0)
	{
		Tmp_file = G_tempfile();
		if (NULL == (Tmp_fd = fopen (Tmp_file, "w+")))
			perror (Tmp_file), exit(1);
		unlink (Tmp_file);
		if (0 > file_cpy(stdin, Tmp_fd))
			exit(1);
		fd = Tmp_fd;
	}
	else
		fd = fopen(input, "r");

	if (fd == NULL)
	{
		perror(input);
		G_usage();
		exit(-1) ;
	}

	if(!gethead(fd, &cellhd, &data_type, &mult, &null_val_str))
	{
		fprintf (stderr, "Can't proceed\n");
		exit(1);
	}

	nrows = cellhd.rows;
	ncols = cellhd.cols;
	if(G_set_window(&cellhd) < 0)
		exit(3);

	if (nrows != G_window_rows())
	{
		fprintf (stderr,
		  "OOPS: rows changed from %d to %d\n", nrows, G_window_rows());
		exit(1);
	}
	if (ncols != G_window_cols())
	{
		fprintf (stderr,
		  "OOPS: cols changed from %d to %d\n", ncols, G_window_cols());
		exit(1);
	}

	rast_ptr = G_allocate_raster_buf(data_type);
	rast = rast_ptr;
	if (G_legal_filename(output) < 0) {
	  sprintf(err, "%s -- illegal output file name", output);
	  G_fatal_error(err);
	  exit(1);
	}
	cf = G_open_raster_new(output, data_type);
	if (cf < 0)
	{
		char msg[100];
		sprintf (msg, "unable to create raster map %s", output);
		G_fatal_error (msg);
		exit(1);
	}
	for (row = 0; row < nrows; row++)
	{
		for (col = 0; col < ncols; col++)
		{
			if (fscanf (fd, "%s", y) != 1)
			{
				char msg[100];
				G_unopen_cell (cf);
				sprintf (msg,
				  "data conversion failed at row %d, col %d\n",
				   row+1, col+1);
				G_fatal_error (msg);
				exit(1);
			}
			if (strcmp(y, null_val_str)) {
			  x = atof(y);
			  G_set_raster_value_d(rast_ptr,
				(DCELL)(x * mult), data_type);
			}
			else {
			   G_set_null_value(rast_ptr, 1, data_type);
			}
			rast_ptr = G_incr_void_ptr(rast_ptr,
					G_raster_size(data_type));
		}
		G_put_raster_row (cf, rast, data_type);
		rast_ptr = rast;
	}
	fprintf (stderr, "CREATING SUPPORT FILES FOR %s\n", output);
	G_close_cell (cf);
	if (title)
		G_put_cell_title (output, title);
	exit (0);
}

int 
file_cpy (FILE *from, FILE *to)
{
	char buf[BUFSIZ];
	long size;
	int  written = 0;

	while (1)
	{
		size = fread (buf, 1, BUFSIZ, from);
		if (!size)
		{
			if (written)
			{
				fflush (to);
				fseek (to, 0l, 0);
			}
			return (0);
		}
		if (!fwrite (buf, 1, size, to))
		{
			perror ("file copy");
			return (-1);
		}
		written = 1;
	}
	/* NOTREACHED */
}
