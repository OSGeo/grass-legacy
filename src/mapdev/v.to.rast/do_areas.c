#include "vtoc.h"
#include "digit.h"
#include "gis.h"

int cur_category;

extern int row_fill ();

/*
** returns 0 or -1 on error 
*/
process_area (map, rows, cols, area, Points)
    struct Map_info *map;
    int rows, cols;
    int area;
    struct line_pnts *Points;
{
    int grid_N, grid_S, grid_E, grid_W;
    int row, col, a, b;

    /* Read in line coordinates */
    /*Points = dig__P_get_area_xy (map, area);
    if (Points == NULL)
    {
	fprintf (stderr, "Get_area returned NULL\n");
	return (-1);
    }
    */
    if (0 >= Vect_get_area_points (map, area, Points))
    {
	fprintf (stderr, "Get_area failed\n");
	return (-1);
    }

/* convert map coords to row_col space leaving x and y as double precision */
    translate (Points->x, Points->y, Points->n_points);

    /* and do it */
    polyfill (Points->x, Points->y, Points->n_points, row_fill);

    return 0;
}
