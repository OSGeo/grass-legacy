/* @(#) 1.11 6/27/90 /usr/grass3.1/src.scs/scspsu/s.do_areas.c */
#include "digit.h"
#include "gis.h"

int cur_category;


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

}
