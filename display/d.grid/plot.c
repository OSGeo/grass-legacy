#include <math.h>
#include "gis.h"
#include "display.h"

int plot_grid (double grid_size, double east, double north)
{
	double x,y;
	double e1,e2;
	struct Cell_head window ;
	double row_dist, colm_dist;

	G_get_set_window (&window);

	/* pull right and bottom edges back one pixel; display lib bug? */
	row_dist = D_d_to_u_row(0) - D_d_to_u_row(1);
	colm_dist = D_d_to_u_col(1) - D_d_to_u_col(0);
	window.south = window.south + row_dist;
	window.east  = window.east  - colm_dist;

	G_setup_plot (
	    D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	    D_move_abs, D_cont_abs);

	/* Draw vertical grids */
	if (window.west > east)
		x = ceil((window.west - east)/grid_size) * grid_size + east ;
	else
		x = east - ceil((east - window.west)/grid_size) * grid_size ;

	while (x <= window.east)
	{
		G_plot_line (x, window.north, x, window.south);
		x += grid_size;
	}

	/* Draw horizontal grids
 *
 * For latlon, must draw in shorter sections
 * to make sure that each section of the grid
 * line is less than half way around the globe
 */
	e1 = (window.east*2 + window.west)/3;
	e2 = (window.west*2 + window.east)/3;

	if (window.south > north)
		y = ceil((window.south - north)/grid_size) * grid_size + north ;
	else
		y = north - ceil((north - window.south)/grid_size) * grid_size ;

	while (y <= window.north)
	{
		G_plot_line (window.east, y, e1, y);
		G_plot_line (e1, y, e2, y);
		G_plot_line (e2, y, window.west, y);
		y += grid_size;
	}

	return 0;
}
