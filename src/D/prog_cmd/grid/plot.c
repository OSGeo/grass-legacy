#include "gis.h"
extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_get_d_east();

extern int D_move_abs();
extern int D_cont_abs();
plot_grid(grid_size)
    int grid_size ;
{
    double ceil();
    double x,y;
    double e1,e2;
    struct Cell_head window ;

    G_get_set_window (&window);

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);

/* Draw vertical grids */
    x = ceil (window.west/grid_size) * grid_size;
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

    y = ceil (window.south / grid_size) * grid_size;
    while (y <= window.north)
    {
	G_plot_line (window.east, y, e1, y);
	G_plot_line (e1, y, e2, y);
	G_plot_line (e2, y, window.west, y);
	y += grid_size;
    }
}
