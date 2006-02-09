#include <grass/gis.h>

int get_east_north (int x, int y, char *east, char *north)
{
    double e, n;
    G_plot_where_en (x, y, &e, &n);
    G_format_easting (e,  east, G_projection());
    G_format_northing (n, north, G_projection());

    return 0;
}
