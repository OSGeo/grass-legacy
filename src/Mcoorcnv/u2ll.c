/* %W%  %G% */
/************************************************************
* name
*    u2ll 
*
* function
*    convert from utm to latitude/longitude 
*
* usage
*    u2ll z=zone e=east n=north s=spheroid
****************************************************************/
#include <stdio.h>

main (argc, argv) char *argv[];
{
    int have_zone, have_east, have_north;
    int have_spheroid_name, have_spheroid_ae, have_spheroid;
    double north,east,lat,lon;
    double a,e;
    char spheroid_name[50];
    char *SPHEROID_PARMS;
    char *CC_spheroid_name();
    char buf[300];
    int zone ;
    int i;

    have_zone = have_east = have_north = 0;
    have_spheroid_name = have_spheroid_ae = have_spheroid = 0;

    for (i = 1; i < argc; i++)
    {
	if (sscanf (argv[i], "z=%d", &zone) == 1)
	{
	    if (have_zone++ || zone == 0)
		usage(argv[0]);
	}
	else if (sscanf (argv[i], "e=%lf", &east) == 1)
	{
	    if (have_east++)
		usage(argv[0]);
	}
	else if (sscanf (argv[i], "n=%lf", &north) == 1)
	{
	    if (have_north++)
		usage(argv[0]);
	}
	else if (sscanf (argv[i], "s=a=%lf,e=%lf", &a, &e) == 2
	   ||    sscanf (argv[i], "s=e=%lf,a=%lf", &e, &a) == 2)
	{
	    if (have_spheroid)
		usage(argv[0]);
	    have_spheroid = have_spheroid_ae = 1;
	    SPHEROID_PARMS = &argv[i][2];
	}
	else if (sscanf (argv[i], "s=%s", buf) == 1)
	{
	    if (have_spheroid)
		usage(argv[0]);
	    strcpy (spheroid_name, buf);
	    have_spheroid = have_spheroid_name = 1;
	}
	else
	    usage (argv[0]);
    }
    if (!have_zone || !have_east || !have_north || !have_spheroid)
	usage(argv[0]);

    if (have_spheroid_name)
    {
	if(CC_u2ll_spheroid (spheroid_name) < 0)
	{
	    fprintf (stderr, "%s: %s - unknown spheroid\n",
		    argv[0], spheroid_name);
	    fprintf (stderr, "known spheroids are\n");
	    for (i = 0; CC_spheroid_name(i); i++)
		fprintf (stderr, "  %s\n", CC_spheroid_name(i));
	    exit(1);
	}
    }
    else
    {
	if(CC_u2ll_spheroid_parameters (a, e)  < 0)
	{
	    fprintf (stderr, "%s: %s illegal spheroid parameters\n",
		argv[0], SPHEROID_PARMS);
	    exit(1);
	}
    }

    CC_u2ll_zone (zone);
    if(CC_u2ll_north (north) < 0)
    {
	fprintf (stderr,
		"%s: northing to far %s\n",
		argv[0], zone<0?"south":"north");
	exit(1);
    }
    if(CC_u2ll (east, &lat, &lon) < 0)
    {
	fprintf (stderr,
		"%s: easting to far from center of zone\n",
		argv[0]);
	exit(1);
    }
    printf ("\n");

    CC_lat_format (lat, buf);
    printf ("lat=%s\n", buf);

    CC_lon_format (lon, buf);
    printf ("lon=%s\n", buf);

    exit(0);
}

static
usage(me) char *me;
{
    fprintf (stderr, "usage %s e=easting n=northing z=zone s=spheroid\n", me);
    exit(1);
}
