#include "glob.h"

lister (name, mapset, text)
    char *name;
    char *mapset;
    char *text;
{
    struct Cell_head window ;
    char *err;
    char *G__get_window();
    char north[20], south[20], east[20], west[20], ns_res[20], ew_res[20];

    if (*name == 0)
    {
	sprintf (text, "%-10s %-10s %-10s %-10s %-7s %-7s",
		"north", "south", "east", "west",
	 	"ns res", "ew res");
	return;
    }

    if(err = G__get_window (&window, window_dir, name, mapset))
    {
	strcpy (text, "** error reading window **");
	return;
    }

    G_format_northing (window.north, north, window.proj);
    G_format_northing (window.south, south, window.proj);
    G_format_easting  (window.east,  east,  window.proj);
    G_format_easting  (window.west,  west,  window.proj);
    G_format_resolution (window.ns_res, ns_res, window.proj);
    G_format_resolution (window.ew_res, ew_res, window.proj);
    sprintf (text, "%-10s %-10s %-10s %-10s %-7s %-7s",
	    north, south, east, west, ns_res, ew_res);

    if (window.north   == cur_window.north  &&
        window.south   == cur_window.south  &&
        window.ns_res  == cur_window.ns_res &&
        window.east    == cur_window.east   &&
        window.west    == cur_window.west   &&
        window.ew_res  == cur_window.ew_res)
	    strcat (text, "*");
}
