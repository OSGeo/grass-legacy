#include "gis.h"
#include "Vect.h"

#define OVERLAP .75

use_plot1(name, mapset)
    char *name, *mapset;
{
    char buf[128];
    double N,S,E,W;
    double G_window_percentage_overlap();
    struct Cell_head window;
    double x;
    struct Map_info Map;

    printf ("Vector file [%s]\n", name);

    /*fd = open_vect (name, mapset);*/

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

    printf ("\n");
    G_format_northing (N, buf, window.proj);
    printf (" North: %s\n", buf);

    G_format_northing (S, buf, window.proj);
    printf (" South: %s\n", buf);

    G_format_easting (E, buf, window.proj);
    printf (" East:  %s\n", buf);

    G_format_easting (W, buf, window.proj);
    printf (" West:  %s\n", buf);
    printf ("\n");

    x =  G_window_percentage_overlap(&window, N, S, E, W);
    /*
    printf ("%.1lf%% overlap\n", x * 100.0);
    */
    return (x > OVERLAP);
}
