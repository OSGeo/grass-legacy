#include "gis.h"
#include "Vect.h"

#define OVERLAP .75

extern int quiet;
int 
use_plot1 (char *name, char *mapset)
{
    char buf[128];
    double N,S,E,W;
    double G_window_percentage_overlap();
    struct Cell_head window;
    double x;
    struct Map_info Map;
	
	if (!quiet)
		fprintf (stdout,"Vector file [%s]\n", name);

    /*fd = open_vect (name, mapset);*/

    Vect_set_open_level (1);
    if (0 > Vect_open_old (&Map, name, mapset))
    {
	sprintf (buf, "Cannot open vector file %s", name);
	G_fatal_error (buf);
    }

    Vect__get_window (&Map, &N, &S, &E, &W);
	if (!quiet)
		Vect_print_header(&Map);
    Vect_close (&Map);

    G_get_set_window (&window);

	if (!quiet) {
		fprintf (stdout,"\n");
		G_format_northing (N, buf, window.proj);
		fprintf (stdout," North: %s\n", buf);

		G_format_northing (S, buf, window.proj);
		fprintf (stdout," South: %s\n", buf);

		G_format_easting (E, buf, window.proj);
		fprintf (stdout," East:  %s\n", buf);

		G_format_easting (W, buf, window.proj);
		fprintf (stdout," West:  %s\n", buf);
		fprintf (stdout,"\n");
	}

    x =  G_window_percentage_overlap(&window, N, S, E, W);
    /*
    fprintf (stdout,"%.1lf%% overlap\n", x * 100.0);
    */
    return (x > OVERLAP);
}
