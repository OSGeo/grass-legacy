#include "vtoc.h"
#include "digit.h"
#include "gis.h"

int cur_category;

extern int row_fill ();

process_area (map, rows, cols, area)
    struct Map_info *map;
    int rows, cols;
    int area;
{
    int grid_N, grid_S, grid_E, grid_W;
    struct line_pnts *Points;
    int row, col, a, b;

    /* Read in line coordinates */
    Points = dig__P_get_area_xy (map, area);
    if (Points == NULL)
    {
	fprintf (stderr, "Get_area returned NULL\n");
	return (-1);
    }

/* convert map coords to row_col space leaving x and y as double precision */
    translate (Points->x, Points->y, Points->n_points);

    /* and do it */
    polyfill (Points->x, Points->y, Points->n_points, row_fill);
}
