#include <stdio.h>
#include <string.h>
#include "glob.h"
#include "G3d.h"
#include "local_proto.h"

int
lister (name, mapset, text)
    char *name;
    char *mapset;
    char *text;
{
    G3D_Region window, cur_window;
    char north[20], south[20], east[20], west[20], ns_res[20], ew_res[20];
    char top[20], bottom[20], tb_res[20];
    char fullName[1000];

    if (*name == 0)
    {
	sprintf (text, "%-9s %-9s %-9s %-9s %-9s %-9s",
		"north", "south", "east", "west", "top", "bottom");
	return -1;
    }

    sprintf (fullName, "%s@%s", name, mapset);
    if(! G3d_readWindow (&window, fullName)) {
      strcpy (text, "** error reading region **");
      return -1;
    }

    G_format_northing (window.north, north, window.proj);
    G_format_northing (window.south, south, window.proj);
    G_format_easting  (window.east,  east,  window.proj);
    G_format_easting  (window.west,  west,  window.proj);
    format_double  (window.top,  top);
    format_double  (window.bottom,  bottom);
    G_format_resolution (window.ns_res, ns_res, window.proj);
    G_format_resolution (window.ew_res, ew_res, window.proj);
    format_double (window.tb_res, tb_res);
    sprintf (text, "%-9s %-9s %-9s %-9s %-9s %-9s",
	    north, south, east, west, top, bottom);

    G3d_getWindow (&cur_window);
    if (window.north   == cur_window.north  &&
        window.south   == cur_window.south  &&
        window.ns_res  == cur_window.ns_res &&
        window.east    == cur_window.east   &&
        window.west    == cur_window.west   &&
        window.top     == cur_window.top    &&
        window.bottom  == cur_window.bottom &&
        window.ew_res  == cur_window.ew_res)
      strcat (text, "*");
    return 0;
}

int
lister2d (name, mapset, text)
    char *name;
    char *mapset;
    char *text;
{
    struct Cell_head window, cur_window;
    char *err;
    char *G__get_window();
    char north[20], south[20], east[20], west[20], ns_res[20], ew_res[20];
    G3D_Region window3d;

    if (*name == 0)
    {
	sprintf (text, "%-10s %-10s %-10s %-10s %-7s %-7s",
		"north", "south", "east", "west",
	 	"ns res", "ew res");
	return -1;
    }

    if((err = G__get_window (&window, window_dir, name, mapset)))
    {
	strcpy (text, "** error reading region **");
	return -1;
    }

    G_format_northing (window.north, north, window.proj);
    G_format_northing (window.south, south, window.proj);
    G_format_easting  (window.east,  east,  window.proj);
    G_format_easting  (window.west,  west,  window.proj);
    G_format_resolution (window.ns_res, ns_res, window.proj);
    G_format_resolution (window.ew_res, ew_res, window.proj);
    sprintf (text, "%-10s %-10s %-10s %-10s %-7s %-7s",
	    north, south, east, west, ns_res, ew_res);

    G3d_getWindow (&window3d);
    G3d_extract2dRegion (&window3d, &cur_window);
    if (window.north   == cur_window.north  &&
        window.south   == cur_window.south  &&
        window.ns_res  == cur_window.ns_res &&
        window.east    == cur_window.east   &&
        window.west    == cur_window.west   &&
        window.ew_res  == cur_window.ew_res)
	    strcat (text, "*");
    return 0;
}
