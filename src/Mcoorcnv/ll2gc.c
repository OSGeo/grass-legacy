/* %W%  %G% */
/************************************************************
* name
*    ll2gc
*
* function
*    convert from latitude/longitude to geocentric
*
* usage
*    ll2gc lat=dd.mm.ss{n|s} lon=dd.mm.ss{e|w} s=spheroid [h=height]
*
*    if the height is missing h=0 is assumed
*          it will computed from the longitude
****************************************************************/
#include <stdio.h>

main (argc, argv) char *argv[];
{
    int have_lat, have_lon, have_height, have_spheroid;
    double x,y,z,lat,lon,h;
    double a,e;
    char *CC_spheroid_name();
    int i;
    char buf[300];

    have_lat = have_lon = have_height = have_spheroid = 0;
    h = 0.0;

    for (i = 1; i < argc; i++)
    {
	if (sscanf (argv[i], "h=%lf", &h) == 1)
	{
	    if (have_height++)
		usage(argv[0]);
	}
	else if (sscanf (argv[i], "lat=%s", buf) == 1)
	{
	    if (have_lat++ || !CC_lat_scan (buf, &lat))
		usage(argv[0]);
	}
	else if (sscanf (argv[i], "lon=%s", buf) == 1)
	{
	    if (have_lon++ || !CC_lon_scan (buf, &lon))
		usage(argv[0]);
	}
	else if (sscanf (argv[i], "s=a=%lf,e=%lf", &a, &e) == 2
	   ||    sscanf (argv[i], "s=e=%lf,a=%lf", &e, &a) == 2)
	{
	    if (have_spheroid)
		usage(argv[0]);
	    if (a <= 0.0 || e < 0.0 || e > 1.0)
	    {
		fprintf (stderr, "%s: %s - illegal spheroid values\n", argv[0],
		    &argv[i][2]);
		exit(1);
	    }
	    have_spheroid = 1;
	}
	else if (sscanf (argv[i], "s=%s", buf) == 1)
	{
	    if (have_spheroid)
		usage(argv[0]);
	    if(!CC_get_spheroid (buf, &a, &e))
	    {
		fprintf (stderr, "%s: %s - unknown spheroid\n",
		    argv[0], buf);
		fprintf (stderr, "known spheroids are\n");
		for (i = 0; CC_spheroid_name(i); i++)
		    fprintf (stderr, "  %s\n", CC_spheroid_name(i));
		exit(1);
	    }
	    have_spheroid = 1;
	}
	else
	    usage(argv[0]);
    }
    if (!have_lat || !have_lon || !have_spheroid)
	usage(argv[0]);

    CC_ll2geo (a, e, lat, lon, h, &x, &y, &z);
    printf ("\nx=%lf\ny=%lf\nz=%lf\n", x, y, z);

    exit(0);
}

static
usage(me) char *me;
{
    fprintf (stderr,"usage: %s lat=dd.mm.ss{n|s} lon=dd.mm.ss{e|w} [h=height] s=spheroid\n", me);
    exit(1);
}
