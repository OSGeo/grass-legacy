/*
****************************************************************************
*
* MODULE:       d.vect.viewproj
* AUTHOR(S):    Sharif Razzaque, LMMS, June 1995
*               Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      To display a vector map in a map projection.
* COPYRIGHT:    (C) 1995 by Lockheed Martin Missiles & Space, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include "config.h"	/* For Grass 5.0 Bev Wallace */
#include "gis.h"
#include "Vect.h"

/* #define OVERLAP .75 */
#define OVERLAP .49


int use_plot1 (char *name, char *mapset)
{
    char buf[128];
    double N,S,E,W;
    double G_window_percentage_overlap();
    struct Cell_head window;
    double x;
    struct Map_info Map;

    fprintf (stdout, "Vector file [%s]\n", name);
    fflush (stdout);

    Vect_set_open_level (1);
    if (0 > Vect_open_old (&Map, name, mapset))
    {
	sprintf (buf, "Cannot open vector file %s", name);
	G_fatal_error (buf);
    }

    Vect__get_window (&Map, &N, &S, &E, &W);
    Vect_print_header(&Map);
    Vect_close (&Map);

    G_get_set_window (&window);

    G_format_northing (N, buf, window.proj);
    fprintf (stdout, "   North: %s\n", buf);

    G_format_northing (S, buf, window.proj);
    fprintf (stdout, "   South: %s\n", buf);

    G_format_easting (E, buf, window.proj);
    fprintf (stdout, "   East:  %s\n", buf);

    G_format_easting (W, buf, window.proj);
    fprintf (stdout, "   West:  %s\n", buf);
    fflush (stdout);

    x =  G_window_percentage_overlap(&window, N, S, E, W);
    fprintf (stdout, "%.1f%% overlap\n", x * 100.0);
    fflush (stdout);

    return (x > OVERLAP);
}
