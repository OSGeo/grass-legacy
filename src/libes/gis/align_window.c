#include "gis.h"
/*
 * char *
 * G_align_window (window, ref)
 *     struct Cell_head *window, *ref;
 *
 * Modifies 'window' to have the same resolution as 'ref'
 * and aligns the window edges (north, south, east, west) to
 * align with the 'ref' grid.
 * 
 * The window is enlarged, if necessary
 * The north is rounded "up", the south "down",
 * the east "right" and the west "left"
 * Lon-lon constraints are taken into consideration
 * to make sure that the north doesn't go above 90 degrees (for lat/lon)
 * or that the east does "wrap" past the west, etc.
 *
 * Returns: NULL if ok,
 *          error message if not (do NOT free this message).
 */
char *
G_align_window (window, ref)
    struct Cell_head *window, *ref;
{
    int preserve;
    double floor(), ceil();
    double G_northing_to_row();
    double G_row_to_northing();
    double G_easting_to_col();
    double G_col_to_easting();
    char *G_adjust_Cell_head();

    window->ns_res = ref->ns_res;
    window->ew_res = ref->ew_res;
    window->zone   = ref->zone;
    window->proj   = ref->proj;

    preserve = window->proj == PROJECTION_LL && window->east == (window->west+360);
    window->south =
	G_row_to_northing (ceil(G_northing_to_row (window->south, ref)), ref);
    window->north =
	G_row_to_northing (floor(G_northing_to_row (window->north, ref)), ref);
    window->east =
	G_col_to_easting (ceil(G_easting_to_col (window->east, ref)), ref);
    window->west =
	G_col_to_easting (floor(G_easting_to_col (window->west, ref)), ref);

    if (window->proj == PROJECTION_LL)
    {
	while (window->north > 90.0)
	    window->north -= window->ns_res;
	while (window->south < -90.0)
	    window->south += window->ns_res;
	if (preserve)
	    window->east = window->west + 360;
	else
	    while (window->east - window->west > 360.0)
		window->east -= window->ew_res;
    }

    return G_adjust_Cell_head (window, 0, 0);
}
