#include "gis.h"

#define OVERLAP .75

use_plot1(name, mapset)
    char *name, *mapset;
{
    FILE *fd, *open_vect() ;
    char buf[128];
    double N,S,E,W;
    double G_window_percentage_overlap();
    struct Cell_head window;
    double x;

    printf ("Vector file [%s in %s]\n", name, mapset);

    fd = open_vect (name, mapset);
    dig__window (fd, &N, &S, &E, &W);
    dig_print_header(fd);
    close_vect(fd);

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
