/* %W%  %G% */
/************************************************************
* name
*    dshift
*
* function
*    convert from latitude/longitude to a different datum
*
* usage
*    dhsift lat=dd.mm.ss{n|s} lon=dd.mm.ss{e|w} is=input-spheroid
*                os=output-spheroid dx=# dy=# dz=#
*
****************************************************************/
#include <stdio.h>

main (argc, argv) char *argv[];
{
    int have_lat, have_lon, have_is, have_os;
    int have_dx, have_dy, have_dz;
    double x,y,z,lat,lon,h;
    double dx,dy,dz;
    double ia,ie;
    double oa,oe;
    char *CC_spheroid_name();
    int i;
    char buf[300];

    have_lat = have_lon = have_is = have_os = 0;
    have_dx = have_dy = have_dz = 0;

    for (i = 1; i < argc; i++)
    {
	if (sscanf (argv[i], "dx=%lf", &dx) == 1)
	{
	    if (have_dx++)
		usage(argv[0]);
	}
	else if (sscanf (argv[i], "dy=%lf", &dy) == 1)
	{
	    if (have_dy++)
		usage(argv[0]);
	}
	else if (sscanf (argv[i], "dz=%lf", &dz) == 1)
	{
	    if (have_dz++)
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
	else if (sscanf (argv[i], "is=a=%lf,e=%lf", &ia, &ie) == 2
	   ||    sscanf (argv[i], "is=e=%lf,a=%lf", &ie, &ia) == 2)
	{
	    if (have_is++)
		usage(argv[0]);
	    if (ia <= 0.0 || ie < 0.0 || ie > 1.0)
	    {
		fprintf (stderr, "%s: %s - illegal spheroid values\n", argv[0],
		    &argv[i][2]);
		exit(1);
	    }
	}
	else if (sscanf (argv[i], "is=%s", buf) == 1)
	{
	    if (have_is++)
		usage(argv[0]);
	    if(!CC_get_spheroid (buf, &ia, &ie))
	    {
		fprintf (stderr, "%s: %s - unknown spheroid\n",
		    argv[0], buf);
		fprintf (stderr, "known spheroids are\n");
		for (i = 0; CC_spheroid_name(i); i++)
		    fprintf (stderr, "  %s\n", CC_spheroid_name(i));
		exit(1);
	    }
	}
	else if (sscanf (argv[i], "os=a=%lf,e=%lf", &oa, &oe) == 2
	   ||    sscanf (argv[i], "os=e=%lf,a=%lf", &oe, &oa) == 2)
	{
	    if (have_os++)
		usage(argv[0]);
	    if (oa <= 0.0 || oe < 0.0 || oe > 1.0)
	    {
		fprintf (stderr, "%s: %s - illegal spheroid values\n", argv[0],
		    &argv[i][2]);
		exit(1);
	    }
	}
	else if (sscanf (argv[i], "os=%s", buf) == 1)
	{
	    if (have_os++)
		usage(argv[0]);
	    if(!CC_get_spheroid (buf, &oa, &oe))
	    {
		fprintf (stderr, "%s: %s - unknown spheroid\n",
		    argv[0], buf);
		fprintf (stderr, "known spheroids are\n");
		for (i = 0; CC_spheroid_name(i); i++)
		    fprintf (stderr, "  %s\n", CC_spheroid_name(i));
		exit(1);
	    }
	}
	else
	    usage(argv[0]);
    }
    if (!(have_lat && have_lon && have_is && have_os && have_dx && have_dy && have_dz))
	usage(argv[0]);

    h = 0.0;
    CC_ll2geo (ia, ie, lat, lon, h, &x, &y, &z);
    x += dx;
    y += dy;
    z += dz;
    CC_geo2ll (oa, oe, x, y, z, &lat, &lon, &h, 20, (double) 1.0e-6);

    CC_lat_format (lat, buf);
    printf ("lat=%s\n", buf);

    CC_lon_format (lon, buf);
    printf ("lon=%s\n", buf);

    exit(0);
}

static
usage(me) char *me;
{
    fprintf (stderr,"Usage:\n\n");
    fprintf (stderr,"  %s lat=dd.mm.ss{n|s} lon=dd.mm.ss{e|w}\n", me);
    fprintf (stderr,"  %*s is=input_spheroid os=output_spheroid\n",
	strlen(me), "");
    fprintf (stderr,"  %*s dx=xshift dy=yshift dz=zshift\n",
	strlen(me), "");
    fprintf (stderr, "\n(enter command as 1 line, not 3)\n");
    exit(1);
}
