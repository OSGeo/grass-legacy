/* %W% %G% */

#include "gis.h"
main(argc,argv) char *argv[];
{
    double lat_nw, lon_nw;
    double lat_ne, lon_ne;
    double lat_sw, lon_sw;
    double lat_se, lon_se;
    double secs;
    double a,e;
    struct Cell_head window;
    char buf[40];

    G_gisinit (argv[0]);

    if (argc != 2)
	usage(argv[0]);
    if (!CC_get_spheroid (argv[1], &a, &e))
	usage(argv[0]);

    G_get_window (&window);
    printf ("\nWINDOW %.2lf N  %.2lf E       ", window.north, window.east);
    printf ("ZONE %d\n", window.zone);
    printf ("       %.2lf S  %.2lf W\n", window.south, window.west);

    CC_u2ll_spheroid_parameters (a,e);
    CC_u2ll_zone (window.zone) ;
    CC_u2ll_north (window.north);
    CC_u2ll (window.west, &lat_nw, &lon_nw);
    CC_u2ll (window.east, &lat_ne, &lon_ne);
    CC_u2ll_north (window.south);
    CC_u2ll (window.west, &lat_sw, &lon_sw);
    CC_u2ll (window.east, &lat_se, &lon_se);

    printf("\n\n");

    CC_lat_format (lat_nw, buf);
    printf ("%-30s",buf);
    CC_lat_format (lat_ne, buf);
    printf ("%s\n",buf);

    CC_lon_format (lon_nw, buf);
    printf ("%-30s",buf);
    CC_lon_format (lon_ne, buf);
    printf ("%s\n",buf);

    printf ("\n\n\n\n\n");

    CC_lat_format (lat_sw, buf);
    printf ("%-30s",buf);
    CC_lat_format (lat_se, buf);
    printf ("%s\n",buf);

    CC_lon_format (lon_sw, buf);
    printf ("%-30s",buf);
    CC_lon_format (lon_se, buf);
    printf ("%s\n",buf);
    printf("\n\n");

    if (secs = lon_nw - lon_ne)
    {
	if (secs < 0) secs = - secs;
	printf ("at northern edge 1 arc second longitude = %lf meters\n",
	    window.ew_res * window.cols / secs);
    }
    if (secs = lon_sw - lon_se)
    {
	if (secs < 0) secs = - secs;
	printf ("at southern edge 1 arc second longitude = %lf meters\n",
	    window.ew_res * window.cols / secs);
    }
    if (secs = lat_nw - lat_sw)
    {
	if (secs < 0) secs = - secs;
	printf ("at western edge 1 arc second latitude = %lf meters\n",
	    window.ns_res * window.rows / secs);
    }
    if (secs = lat_ne - lat_se)
    {
	if (secs < 0) secs = - secs;
	printf ("at eastern edge 1 arc second latitude = %lf meters\n",
	    window.ns_res * window.rows / secs);
    }
}

usage (me) char *me;
{
    char *CC_spheroid_name();
    int i;

    fprintf (stderr, "usage: %s spheroid\n", me);
    fprintf (stderr, "where spheroid is one of\n");
    for (i = 0; CC_spheroid_name(i); i++)
    {
	if (i%3 == 0) fprintf (stderr, "  ");
	fprintf (stderr, "%-20s ", CC_spheroid_name(i));
	if (i%3 == 2) fprintf (stderr, "\n");
    }
    fprintf (stderr, "\n");
    exit (1);
}
