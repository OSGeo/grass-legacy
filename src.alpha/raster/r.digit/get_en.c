get_east_north (x, y, east, north)
    char *east, *north;
{
    double e, n;
    G_plot_where_en (x, y, &e, &n);
    G_format_easting (e,  east, G_projection());
    G_format_northing (n, north, G_projection());
}
