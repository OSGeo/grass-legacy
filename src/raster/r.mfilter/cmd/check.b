#include "gis.h"
/************************************************************
 * check_window: check the window against a data layer cell header
 *   die or issue warning if window res not same as cell resolution
 ***********************************************************/

check_window (name, mapset, check_level)
    char *name;
    char *mapset;
{
    struct Cell_head cellhd;
    struct Cell_head window;
    char msg[100];

    if (check_level > 1) return 1;

    G_get_window (&window);
    if (G_get_cellhd (name, mapset, &cellhd) < 0)
    {
	sprintf (msg, "can't read header for [%s] in [%s]", name, mapset);
	G_fatal_error (msg);
    }
    if (window.ns_res == cellhd.ns_res &&
        window.ew_res == cellhd.ew_res)
		return 1;
    sprintf (msg,"region resolution differs from resolution for raster file [%s] in [%s]",name,mapset);
    if (check_level == 0)
	G_fatal_error (msg);
    else
	G_warning (msg);
    return 0;
}
