#include <string.h>
#include <unistd.h>
#include "gis.h"
#define MAIN
#include "ncb.h"
#include "method.h"
#include "local_proto.h"

/*
 * July 99 - BB - added RASTER_MAP_TYPE args to methods in order
 * to eliminate integer truncation allowance ( + .5 ) when
 * map_type != CELL
 */

int main (int argc, char *argv[])
{
	char *p;
	int method;
	int verbose;
	int in_fd;
	int out_fd;
	void *rp;
	void *result;
	RASTER_MAP_TYPE map_type;
	int row, col;
	int readrow;
	int nrows, ncols;
	int n;
	int copycolr;
	cfunc newvalue;
	ifunc cat_names;
	struct Colors colr;
	struct Cell_head cellhd;
	struct Cell_head window;
	struct GModule *module;
	struct
	    {
		struct Option *input, *output;
		struct Option *method, *size;
		struct Option *title;
	} parm;
	struct
	    {
		struct Flag *quiet, *align;
	} flag;

	DCELL *values;   /* list of neighborhood values */

	G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Makes each cell category value a "
		"function of the category values assigned to the cells "
		"around it, and stores new cell values in an output raster "
		"map layer.";

	parm.input = G_define_option() ;
	parm.input->key        = "input" ;
	parm.input->type       = TYPE_STRING ;
	parm.input->required   = YES ;
	parm.input->gisprompt  = "old,cell,raster" ;
	parm.input->description= "Name of existing raster file" ;

	parm.output = G_define_option() ;
	parm.output->key        = "output" ;
	parm.output->type       = TYPE_STRING ;
	parm.output->required   = YES ;
	parm.output->gisprompt  = "any,cell,raster" ;
	parm.output->description= "Name of the new raster file" ;

	parm.method = G_define_option() ;
	parm.method->key        = "method" ;
	parm.method->type       = TYPE_STRING ;
	parm.method->required   = YES ;
	p = parm.method->options  = G_malloc(1024);
	for (n = 0; menu[n].name; n++)
	{
		if (n)
			strcat (p, ",");
		else
			*p = 0;
		strcat (p, menu[n].name);
	}
	parm.method->description= "Neighborhood operation" ;

	parm.size = G_define_option() ;
	parm.size->key        = "size" ;
	parm.size->type       = TYPE_INTEGER ;
	parm.size->required   = YES ;
	parm.size->options    = "1,3,5,7,9,11,13,15,17,19,21,23,25" ;
	parm.size->description= "Neighborhood size" ;

	parm.title = G_define_option() ;
	parm.title->key        = "title" ;
	parm.title->key_desc   = "\"phrase\"" ;
	parm.title->type       = TYPE_STRING ;
	parm.title->required   = NO ;
	parm.title->description= "Title of the output raster file" ;

	flag.align = G_define_flag();
	flag.align->key = 'a';
	flag.align->description = "Do not align output with the input";

	flag.quiet = G_define_flag();
	flag.quiet->key = 'q';
	flag.quiet->description = "Run quietly";

	if (G_parser(argc,argv))
		exit(1);

	p = ncb.oldcell.name = parm.input->answer;
	if(NULL == (ncb.oldcell.mapset = G_find_cell2(p,"")))
	{
		fprintf (stderr, "%s: <%s> raster file not found\n",
		    G_program_name(), p);
		exit(1);
	}
	p = ncb.newcell.name = parm.output->answer;
	if (G_legal_filename(p) < 0)
	{
		fprintf (stderr, "%s: <%s> illegal file name\n",
		    G_program_name(), p);
		exit(1);
	}
	ncb.newcell.mapset = G_mapset();

	if(!flag.align->answer)
	{
		if (G_get_cellhd (ncb.oldcell.name, ncb.oldcell.mapset, &cellhd) < 0)
			exit(1);
		G_get_window (&window);
		G_align_window (&window, &cellhd);
		G_set_window (&window);
	}

	nrows = G_window_rows();
	ncols = G_window_cols();

	/* open cell files */
	if ((in_fd = G_open_cell_old (ncb.oldcell.name, ncb.oldcell.mapset)) < 0)
	{
		char msg[200];
		sprintf(msg,"can't open cell file <%s> in mapset %s\n",
			ncb.oldcell.name, ncb.oldcell.mapset);
		G_fatal_error (msg);
		exit(-1);
	}

	map_type = G_raster_map_type(ncb.oldcell.name, ncb.oldcell.mapset);

	/* get the method */
	for (method = 0; p = menu[method].name; method++)
		if (strcmp(p, parm.method->answer) == 0)
			break;
	if (!p)
	{
		fprintf (stderr, "<%s=%s> unknown %s\n",
		    parm.method->key, parm.method->answer, parm.method->key);
		G_usage();
		exit(1);
	}

	/* copy color table? */
	copycolr = menu[method].copycolr;
	if (copycolr)
	{
		G_suppress_warnings (1);
		copycolr = (G_read_colors (ncb.oldcell.name, ncb.oldcell.mapset, &colr) > 0);
		G_suppress_warnings (0);
	}

	/* get the neighborhood size */
	sscanf (parm.size->answer, "%d", &ncb.nsize);
	ncb.dist = ncb.nsize/2;

	/* allocate the cell buffers */
	allocate_bufs ();
	values = (DCELL *) G_malloc (ncb.nsize * ncb.nsize * sizeof (DCELL));
	result = G_allocate_raster_buf (map_type);


	/* get title, initialize the category and stat info */
	if (parm.title->answer)
		strcpy (ncb.title, parm.title->answer);
	else
		sprintf (ncb.title,"%dx%d neighborhood: %s of %s",
		    ncb.nsize, ncb.nsize, menu[method].name, ncb.oldcell.name);


	/* initialize the cell bufs with 'dist' rows of the old cellfile */

	readrow = 0;
	for (row = 0; row < ncb.dist; row++)
		readcell (in_fd, readrow++, nrows, ncols);

	/* establish the newvalue routine */
	newvalue = menu[method].method;

	/* open cell file */
	in_fd = G_open_cell_old (ncb.oldcell.name, ncb.oldcell.mapset);
	if (in_fd < 0)
		exit(1);

	/*open the new cellfile */
	out_fd = G_open_raster_new (ncb.newcell.name, map_type);
	if (out_fd < 0)
		exit(1);

	if (verbose = !flag.quiet->answer)
		fprintf (stderr, "Percent complete ... ");
	ncb.changed = 0;
	for (row = 0; row < nrows; row++)
	{
		if (verbose)
			G_percent (row, nrows, 2);
		readcell (in_fd, readrow++, nrows, ncols);
		ncb.center = ncb.buf[ncb.dist] + ncb.dist;
		rp = result;
		for (col = 0; col < ncols; col++)
		{
			n = gather (values, col);
		        G_set_raster_value_d(rp, newvalue (values, n, map_type), map_type);
			rp = G_incr_void_ptr(rp, G_raster_size(map_type));
			ncb.center++;
		}
		G_put_raster_row (out_fd, result, map_type);
	}
	if (verbose)
		G_percent (row, nrows, 2);

	G_close_cell (out_fd);
	G_close_cell (in_fd);

	/* put out category info */
	null_cats () ;
	if (cat_names = menu[method].cat_names)
		cat_names();
	G_set_cat ((CELL)0, "no data", &ncb.cats);
	G_write_cats (ncb.newcell.name, &ncb.cats);


	if(copycolr)
		G_write_colors (ncb.newcell.name, ncb.newcell.mapset, &colr);

	exit(0);
}
