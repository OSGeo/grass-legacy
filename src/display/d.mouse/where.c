#include "gis.h"

where (east0, north0, drag)
    double east0, north0;
    char *drag;
{
    char ebuf[50], nbuf[50];
    int x, y ;
    int x0,y0 ;
    double east, north ;
    int button ;

    G_plot_where_xy (east0, north0, &x0, &y0);

    if (strcmp (drag, "box") == 0)
	R_get_location_with_box(x0, y0, &x, &y, &button) ;
    else if (strcmp (drag, "line") == 0)
	R_get_location_with_line(x0, y0, &x, &y, &button) ;
    else
	R_get_location_with_pointer(&x, &y, &button) ;

    G_plot_where_en (x, y, &east, &north);
    G_format_easting  (east,  ebuf, G_projection());
    G_format_northing (north, nbuf, G_projection());
    fprintf (stdout,"%s %s %d\n", ebuf, nbuf, button) ;
}
