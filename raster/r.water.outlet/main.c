/* this program makes a watershed basin raster map using the drainage pointer map
** from an outlet point defined by a easting and a northing.
** last modified 01/10/92
** by  Chuck Ehlschlaeger

Help text inserted by 
 Markus Neteler 12/97
*/

#include <stdio.h>
#define MAIN
#include "basin.h"
#include "outletP.h"
#undef MAIN

int main (int argc, char *argv[])
{
	double	N,E;
	int	row, col, basin_fd, drain_fd;
	CELL	*cell_buf;
	char	drain_name[80], *drain_mapset, E_f, dr_f, ba_f, N_f,  errr;
	struct GModule *module;
        struct Option *opt1, *opt2, *opt3, *opt4;
	char buf[400];

	module = G_define_module();
	module->description =
		"Watershed basin creation program.";

	opt1 = G_define_option() ;
	opt1->key        = "drainage" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->gisprompt  = "any,cell,raster" ;
	opt1->description= "Name of input raster map" ;

	opt2 = G_define_option() ;
	opt2->key        = "basin" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = YES ;
	opt2->gisprompt  = "any,cell,raster" ;
	opt2->description= "Name of raster map to contain results" ;

	opt3 = G_define_option() ;
	opt3->key        = "easting" ;
	opt3->type       = TYPE_STRING ;
	opt3->key_desc   = "x" ;
	opt3->multiple   = NO;
	opt3->required   = YES ;
	opt3->description= "The map E  grid coordinates" ;

	opt4 = G_define_option() ;
	opt4->key        = "northing" ;
	opt4->type       = TYPE_STRING ;
	opt4->key_desc   = "y" ;
	opt4->multiple   = NO;
	opt4->required   = YES ;
	opt4->description= "The map N grid coordinates" ;


	G_gisinit(argv[0]);
	if (G_get_window(&window) < 0)
	{
		sprintf (buf,"can't read current window parameters");
		G_fatal_error (buf);
		exit(1);
	}

	/*   Parse command line */
	if (G_parser(argc, argv))
		exit(-1);

                    strcpy (drain_name, opt1->answer);
                    strcpy (basin_name, opt2->answer);
                    if(!G_scan_easting(*opt3->answers, &E, G_projection()))
		{
			fprintf (stderr, "Illegal east coordinate <%s>\n",
					 *opt3->answer);
			G_usage();
			exit(1);
		}
	if(!G_scan_northing(*opt4->answers, &N, G_projection()))
		{
			fprintf (stderr, "Illegal north coordinate <%s>\n",
					 *opt4->answer);
			G_usage();
			exit(1);
		}

	if(E < window.west ||  E > window.east ||  N < window.south ||  N > window.north)
		{
			fprintf(stderr,"Warning, ignoring point outside window: \n") ;
			fprintf(stderr,"   %.4f,%.4f\n", E, N) ;
		}

	G_get_set_window (&window);
	dr_f = ba_f = N_f = E_f = errr = 0;
 
	drain_mapset = do_exist (drain_name);
	do_legal (basin_name);
	nrows = G_window_rows();
	ncols = G_window_cols();
	total = nrows * ncols;
	nrows_less_one = nrows - 1;
	ncols_less_one = ncols - 1;
	drain_fd = G_open_cell_old (drain_name, drain_mapset);
	if (drain_fd < 0) G_fatal_error ("unable to open drainage pointer map");
	drain_ptrs = (char *) G_malloc (sizeof(char) * size_array (&pt_seg, nrows, ncols));
	bas = (CELL *) G_calloc (size_array (&ba_seg, nrows, ncols), sizeof(CELL));
	cell_buf = G_allocate_cell_buf();
	for (row = 0; row < nrows; row++) {
		G_get_map_row (drain_fd, cell_buf, row);
		for (col = 0; col < ncols; col++) {
			if (cell_buf[col] == 0)
				total--;
			drain_ptrs[SEG_INDEX(pt_seg, row, col)] = cell_buf[col];
		}
	}
	G_free (cell_buf);
	row = (window.north - N) / window.ns_res;
	col = (E - window.west) / window.ew_res;
	if (row >= 0 && col >= 0 && row < nrows && col < ncols)
		overland_cells (row, col);
	G_free (drain_ptrs);
	cell_buf = G_allocate_cell_buf();
	basin_fd = G_open_cell_new (basin_name);
	if (basin_fd < 0)
		G_fatal_error ("unable to open new basin map");
	for (row = 0; row < nrows; row++) {
		for (col = 0; col < ncols; col++) {
		    cell_buf[col] = bas[SEG_INDEX(ba_seg, row, col)];
		}
		G_put_map_row (basin_fd, cell_buf);
	}
	G_free (bas);
	G_free (cell_buf);
	if (G_close_cell (basin_fd) < 0) 
		G_fatal_error ("unable to close new basin map layer");

	return 0;
}
