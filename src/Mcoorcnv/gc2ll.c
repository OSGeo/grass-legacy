/* %W%  %G% */
/************************************************************
* name
*    gc2ll
*
* function
*    convert from geocentric to latitude/longitude
*
* usage
*    gc2ll x=# y=# z=# s=spheroid
****************************************************************/
#include <stdio.h>

main (argc, argv) char *argv[];
{
    int have_x, have_y, have_z;
    int have_spheroid;
    double x,y,z,lat,lon,h;
    double a,e;
    double stop;
    double iterations;
    char *CC_spheroid_name();
    int i;
    char buf[300];

    have_x = have_y = have_z = 0;
    have_spheroid = 0;
    stop = 1e-6;
    iterations = 20;

    for (i = 1; i < argc; i++)
    {
	if (sscanf (argv[i], "x=%lf", &x) == 1)
	{
	    if (have_x++)
		usage(argv[0]);
	}
	else if (sscanf (argv[i], "y=%lf", &y) == 1)
	{
	    if (have_y++)
		usage(argv[0]);
	}
	else if (sscanf (argv[i], "z=%lf", &z) == 1)
	{
	    if (have_z++)
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
	else if (sscanf (argv[i], "stop=%lf", &stop) == 1)
		;
	else if (sscanf (argv[i], "interations=%d", &iterations) == 1)
		;
	else
	    usage(argv[0]);
    }
    if (!have_x || !have_y || !have_z)
	usage(argv[0]);

    i = CC_geo2ll (a, e, x, y, z, &lat, &lon, &h, iterations, stop);
    printf ("\n");
    CC_lat_format (lat, buf);
    printf ("lat=%s\n", buf);
    CC_lon_format (lon, buf);
    printf ("lon=%s\n", buf);
    printf ("h=%lf\n", h);
    if (i == 0)
	fprintf (stderr,"\nnote: did not converge in %d iterations with stop=%lg\n",
		iterations, stop);

    exit(i);
}

static
usage(me) char *me;
{
    fprintf (stderr,"usage: %s x=# y=# z=# s=spheroid\n", me);
    exit(1);
}
