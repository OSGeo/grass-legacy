#include <string.h>
#include "gis.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
	struct History hist;
	struct Categories cats;
	struct Categories newcats;
	struct Colors colr;
	struct Cell_head cellhd;
	struct Range range;
	int hist_ok, colr_ok, cats_ok;
	char name[100], *mapset;
	char result[100];
	void *rast;
	int nrows, ncols;
	int row;
	int infd, outfd;
	int verbose;
	char buf[256] ;
	RASTER_MAP_TYPE data_type, out_type ;
	struct GModule *module;
	struct
	{
	  struct Option *input, *output;
	} option ;
	struct Flag *flag1 ;

    module = G_define_module();
    module->description =
		"GRASS raster map layer data resampling capability.";
					        
	/* Define the different options */

	option.input = G_define_option() ;
	option.input->key        = "input";
	option.input->type       = TYPE_STRING;
	option.input->required   = YES;
	option.input->gisprompt  = "old,cell,raster" ;
	option.input->description= "Name of an input layer" ;

	option.output = G_define_option() ;
	option.output->key        = "output";
	option.output->type       = TYPE_STRING;
	option.output->required   = YES;
	option.output->gisprompt  = "new,cell,raster" ;
	option.output->description= "Name of an output layer";

	/* Define the different flags */

	flag1 = G_define_flag() ;
	flag1->key         = 'q' ;
	flag1->description = "Quiet" ;

	G_gisinit(argv[0]);
	if (G_parser(argc, argv))
		exit (-1);

	strcpy (name, option.input->answer);
	strcpy (result, option.output->answer);
	verbose = (! flag1->answer);

        mapset = G_find_cell2 (name, "");
        if (mapset == NULL)
        {
                char buf[200];
                sprintf (buf, "cell file [%s] not found", name);
                G_fatal_error (buf);
        }

        if (G_legal_filename (result) < 0)
        {
                char tmp_buf[200];

                sprintf (tmp_buf, "[%s] illegal name", result);
                G_fatal_error (tmp_buf);
        }

	/* determine the map type;
	   data_type is the type of data being processed,
	   out_type is the type of map being created. */
	data_type = G_raster_map_type(name, mapset);
	out_type = data_type;

	hist_ok = G_read_history (name, mapset, &hist) >= 0;
	colr_ok = G_read_colors (name, mapset, &colr) > 0;
	cats_ok = G_read_cats (name, mapset, &cats) >= 0;
	if (cats_ok)
	{
	   G_unmark_raster_cats (&cats);
 	   G_init_cats((CELL)0,G_get_cats_title(&cats),&newcats);
        }
		
	infd = G_open_cell_old (name, mapset);
	if (infd < 0)
		exit(1);

	if (G_get_cellhd (name, mapset, &cellhd) < 0)
		exit(1);

	/* raster buffer is big enough to hold data */
	rast = G_allocate_raster_buf(data_type);
	nrows = G_window_rows();
	ncols = G_window_cols();
	if(ncols <= 1)
	  rast = (void *)G_realloc((char *) rast, 2 * G_raster_size(data_type));
	  /* we need the buffer at least 2 cells large */

	outfd = G_open_raster_new (result, out_type);
	G_set_null_value(rast, ncols, out_type);

	if (outfd < 0)
		exit(1);

	if (verbose)
		fprintf (stderr, "percent complete: ");

	for (row = 0; row < nrows; row++)
	{
		if (verbose)
			G_percent (row, nrows, 2);
		if (G_get_raster_row (infd, rast, row, data_type) < 0)
			exit(1);
		if (G_put_raster_row (outfd, rast, out_type) < 0)
			exit(1);
		G_mark_raster_cats (rast, ncols, &cats, data_type);
	}

	if (verbose)
		G_percent (row, nrows, 2);

	G_close_cell (infd);

	if (verbose)
		fprintf (stdout, "Creating support files for %s\n", result);

	G_close_cell (outfd);

        G_rewind_raster_cats (&cats);

	if (cats_ok) {
		long count;
		void *rast1, *rast2;

		rast1 = rast; 
		rast2 = G_incr_void_ptr(rast, G_raster_size(data_type));

                fprintf(stdout, "creating new cats file...\n");
		while (G_get_next_marked_raster_cat(&cats, 
			     rast1, rast2, &count, data_type)) 
		   G_set_raster_cat(rast1, rast2, 
		      G_get_raster_cat(rast1, &cats, data_type), 
		      &newcats, data_type);
			
		G_write_cats (result, &newcats);
		G_free_cats (&cats);
		G_free_cats (&newcats);
		}

	if (colr_ok)
	{
		if(G_read_range (result, G_mapset(), &range) > 0)
		{
		    CELL min, max, cmin, cmax;
		    G_get_range_min_max (&range, &min, &max);
		    G_get_color_range (&cmin, &cmax, &colr);
		    if (min > cmin) cmin = min;
		    if (max < cmax) cmax = max;
		    G_set_color_range (cmin, cmax, &colr);
		}
		G_write_colors (result, G_mapset(), &colr);
	}

	if (hist_ok)
		G_write_history (result, &hist);

	exit(0);
}
