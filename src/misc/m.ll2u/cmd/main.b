/************************************************************
* name
*    m.ll2u
*
* function
*    convert from latitude/longitude to utm
*
* usage
*    ll2u -z spheroid=name [zone=value] [input=file] [output=file]
*
*    if the zone is missing or specified as 0, it will be
*    computed from the longitude
*
* coordinates are read from input, format: lon lat anything
*
* output for each input line is: east west [zone] anything
*   [zone] is suppressed if -z specified
*
*  lon format is ddd:mm:ss{E|W}
*  lat format is dd:mm:ss{N|S}
*
****************************************************************/
#include "gis.h"

main (argc, argv) char *argv[];
{
    struct
    {
	struct Option *spheroid, *zone, *input, *output;
    } parm;
    struct
    {
	struct Flag *z;
    } flag;
    int n, len;
    char *G_ellipsoid_name();
    char *name;
    char *opt;
    double north,east,lat,lon;
    double a,e;
    int zone,z ;
    int want_zone;
    int print_zone;
    char buf[1024];
    char ebuf[256], nbuf[256], label[512];

    G_gisinit(argv[0]);

    parm.spheroid = G_define_option();
    parm.spheroid->key = "spheroid";
    parm.spheroid->description = "reference spheroid (ellipsoid)";
    parm.spheroid->type = TYPE_STRING;
    parm.spheroid->required = YES;
    len=0;
    for (n=0; name = G_ellipsoid_name(n); n++)
	len += strlen(name)+1;
    parm.spheroid->options = opt = G_malloc(len);
    for (n=0; name = G_ellipsoid_name(n); n++)
    {
	if (n) strcat (opt, ",");
	else *opt = 0;
	strcat (opt, name);
    }

    parm.zone = G_define_option();
    parm.zone->key = "zone";
    parm.zone->description = "utm zone (results will be forced into this zone)";
    parm.zone->type = TYPE_INTEGER;
    parm.zone->required = NO;
    parm.zone->options = "1-60";
	/* note: on output southern hemisphere zones are negative */

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->description = "input file";
    parm.input->type = TYPE_STRING;
    parm.input->required = NO;

    parm.output = G_define_option();
    parm.output->key = "output";
    parm.output->description = "output file";
    parm.output->type = TYPE_STRING;
    parm.output->required = NO;

    flag.z = G_define_flag();
    flag.z->key = 'z';
    flag.z->description = "Suppress printing the utm zone";

    if (G_parser(argc,argv))
	exit(1);
    
    zone = 0;
    if (parm.zone->answer)
	sscanf (parm.zone->answer, "%d", &zone);

    if(!G_get_ellipsoid_by_name(parm.spheroid->answer, &a, &e))
    {
	fprintf (stderr, "%s=%s - unknown spheroid\n",
		parm.spheroid->key, parm.spheroid->answer);
	G_usage();
	exit(1);
    }
    CC_u2ll_spheroid_parameters(a,e);

    if (parm.input->answer)
    {
	if (freopen(parm.input->answer, "r", stdin)==NULL)
	{
	    fprintf (stderr, "%s: %s=", G_program_name(), parm.input->key);
	    perror (parm.input->answer);
	    exit(1);
	}
    }

    if (parm.output->answer)
    {
	if (freopen(parm.output->answer, "w", stdout)==NULL)
	{
	    fprintf (stderr, "%s: %s=", G_program_name(), parm.output->key);
	    perror (parm.output->answer);
	    exit(1);
	}
    }

    want_zone = !flag.z->answer;
    if (isatty(0))
    {
	printf ("Enter lon lat, one per line, in the format\n");
	printf (" %s %s\n", G_lon_format_string(), G_lat_format_string());
	printf ("Enter the word <end> when done\n");
    }
    for (n=1; input(buf); n++)
    {
	switch (sscanf(buf, "%s %s %[^\n]", ebuf, nbuf, label))
	{
	case 1: error (n, buf, "must specify lon lat"); continue;
	case 2: *label = 0; break;
	case 3: break;
	default: continue;
	}
	if (!G_scan_easting (ebuf, &lon, PROJECTION_LL))
	{
	    error (n, ebuf, "illegal longitude"); continue;
	}
	if (!G_scan_northing (nbuf, &lat, PROJECTION_LL))
	{
	    error (n, ebuf, "illegal latitude"); continue;
	}
	lat *= 3600.0; /* convert to arc seconds for CC library */
	lon *= 3600.0; /* convert to arc seconds for CC library */
	lon = -lon;    /* CC lib expects negative in the west, which is
			  reverse from what G_scan_easting() returns */

	z = zone; /* parser ensures that zone is never negative */
	if (z != 0 && lat < 0)
	    z = -z;

	switch (CC_ll2u (lat, lon, &east, &north, &z))
	{
	case -1:
	    error (n, nbuf, lat<0 ? "too far south" : "too far north");
	    continue;
	case -2:
	    error (n, ebuf, "too far from center of utm zone");
	    continue;
	}

	G_format_easting (east, ebuf, PROJECTION_UTM);
	G_format_northing (north, nbuf, PROJECTION_UTM);
	printf ("%s %s ", ebuf, nbuf);
	if (want_zone)
	    printf ("%d ", z);
	printf ("%s\n", label);
	if (isatty(0) && !isatty(1))
	{
	    fprintf (stderr, "%s %s ", ebuf, nbuf);
	    if (want_zone)
		fprintf (stderr, "%d ", z);
	    fprintf (stderr, "%s\n", label);
	}
    }
    exit(0);
}

input(buf)
    char *buf;
{
    if (isatty(0))
	printf ("> ");
    if(!gets(buf))
	return 0;
    G_strip (buf);
    if (strcmp(buf, "end") == 0) return 0;
    return 1;
}

error (line, buf, msg)
    char *buf, *msg;
{
    printf ("** line %d: %s - %s **\n", line, buf, msg);
    if (!isatty(1))
    {
	if (!isatty(0))
	    fprintf (stderr, "%s: ", G_program_name());
	fprintf (stderr, "** line %d: %s - %s **\n", line, buf, msg);
    }
}
