/* %W%  %G% */
/************************************************************
* name
*    ll2u
*
* function
*    convert from latitude/longitude to utm
*
* usage
*    ll2u lat=dd.mm.ss{n|s} lon=dd.mm.ss{e|w} [z=zone] s=spheroid
*
*    if the zone is missing or specified as 0,
*          it will computed from the longitude
****************************************************************/
#include <stdio.h>

main (argc, argv) char *argv[];
{
    int have_lat, have_lon, have_zone;
    int have_spheroid_name, have_spheroid_ae, have_spheroid;
    double north,east,lat,lon;
    double a,e;
    int zone ;
    char *CC_spheroid_name();
    int i;
    char *LON, *LAT, *SPHEROID_PARMS;
    char spheroid_name[50];
    char buf[300];

    have_lat = have_lon = have_zone = 0;
    have_spheroid_name = have_spheroid_ae = have_spheroid = 0;
    zone = 0;

    for (i = 1; i < argc; i++)
    {
	if (sscanf (argv[i], "z=%d", &zone) == 1)
	{
	    if (have_zone++)
		usage(argv[0]);
	}
	else if (sscanf (argv[i], "lat=%s", buf) == 1)
	{
	    if (have_lat++ || !CC_lat_scan (buf, &lat))
		usage(argv[0]);
	    LAT = argv[i];
	}
	else if (sscanf (argv[i], "lon=%s", buf) == 1)
	{
	    if (have_lon++ || !CC_lon_scan (buf, &lon))
		usage(argv[0]);
	    LON = argv[i];
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
	    usage(argv[0]);
    }
    if (!have_lat || !have_lon || !have_spheroid)
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
	if (CC_u2ll_spheroid_parameters (a, e) < 0)
	{
	    fprintf (stderr, "%s: %s illegal spheroid parameters\n",
		argv[0], SPHEROID_PARMS);
	    exit(1);
	}
    }

    switch (CC_ll2u (lat, lon, &east, &north, &zone))
    {
    case -1:
	fprintf (stderr, "%s: %s too far %s\n",
	    argv[0], LAT, lat<0?"south":"north");
	exit(1);
    case -2:
	fprintf (stderr,
	    "%s: %s too far from center of utm zone\n", argv[0], LON);
	exit(1);
    }
    printf ("e=%lf\n", east);
    printf ("n=%lf\n", north);
    printf ("z=%d\n", zone);

    exit(0);
}

static
usage(me) char *me;
{
    fprintf (stderr,
	"usage: %s lat=dd.mm.ss{n|s} lon=dd.mm.ss{e|w} s=spheroid [z=zone]\n", me);
    exit(1);
}
