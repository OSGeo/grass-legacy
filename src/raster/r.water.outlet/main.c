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

	G_gisinit(argv[0]);
	G_get_set_window (&window);
	dr_f = ba_f = N_f = E_f = errr = 0;
        for (row = 1; row < argc; row++) {
		if 	(sscanf (argv[row], "dr=%[^\n]", drain_name) == 1) 
			dr_f++;
		else if (sscanf (argv[row], "ba=%[^\n]", basin_name) == 1)
			ba_f++;
		else if (sscanf (argv[row], "n=%lf", &N) == 1)
			N_f++;
		else if (sscanf (argv[row], "e=%lf", &E) == 1)
			E_f++;
		else {
			printf ("%s bad input: [%s]\n", G_program_name(), argv[row]);
			errr++;
		}
	}
        /* Nothing was set! inserted by Markus Neteler 12/97*/
        if(!N_f) {
           if(!E_f) {
              if(!ba_f) { G_fatal_error(
		    "No parameters specified!\nThis module creates a watershed basin (catchment area) raster map using a drainage pointer map (create with:\nr.watershed elevation=elevfile drainage=drainage) from an outlet point\ndefined by easting and northing (use d.what.rast).\nusage: %s dr=drainage_map ba=basin_map n=outlet_northing e=outlet_easting\n", 
		    G_program_name());
	                }
                    }
                 }

        /* One or more isn't set */
        if(!N_f) {
		printf ("%s missing n=outlet_northing\n", G_program_name()); errr++;
	}
	if(!E_f) {
		printf ("%s missing e=outlet_easting\n", G_program_name()); errr++;
	}
	if(!ba_f) {
		printf ("%s missing ba=basin_map to be created\n", G_program_name());
		errr++;
	}
	if (errr) {
		G_fatal_error(
"usage: %s dr=drainage_map ba=basin_map n=outlet_northing e=outlet_easting\n", 
		    G_program_name());
	}
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
