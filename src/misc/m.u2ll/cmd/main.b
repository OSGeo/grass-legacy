/************************************************************
* name
*    m.u2ll
*
* function
*    convert from utm to latitude/longitude
*
* usage
*    ll2u spheroid=name [zone=value] [input=file] [output=file]
*
*    if the zone is missing it will be read from the current database
*
* coordinates are read from input, format: east northing anything
*
* output for each input line is: lon lat anything
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
	struct Flag *southern;
    } flag;
    int n, len;
    char *G_ellipsoid_name();
    char *name;
    char *opt;
    double north,east,lat,lon;
    double a,e;
    int zone ;
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
    parm.zone->description = "utm zone";
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

    flag.southern = G_define_flag();
    flag.southern->key = 's';
    flag.southern->description = "Specified zone is in the southern hemisphere";

    if (G_parser(argc,argv))
	exit(1);
    
    if (parm.zone->answer)
    {
	sscanf (parm.zone->answer, "%d", &zone);
	if (flag.southern) zone = -zone;
    }
    else
    {
	if (G_projection() != PROJECTION_UTM)
	{
	    fprintf (stderr, "%s is not a UTM database. You must specify zone=\n",
		G_location());
	    G_usage();
	    exit(1);
	}
	zone = G_zone();
    }

    if(!G_get_ellipsoid_by_name(parm.spheroid->answer, &a, &e))
    {
	fprintf (stderr, "%s=%s - unknown spheroid\n",
		parm.spheroid->key, parm.spheroid->answer);
	G_usage();
	exit(1);
    }
    CC_u2ll_spheroid_parameters(a,e);
    CC_u2ll_zone (zone);

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

    if (isatty(0))
    {
	printf ("Enter easting northing, one per line\n");
	printf ("Enter the word <end> when done\n");
    }
    for (n=1; input(buf); n++)
    {
	switch (sscanf(buf, "%lf %lf %[^\n]", &east, &north, label))
	{
	case 1: error (n, buf, "must specify easting northing"); continue;
	case 2: *label = 0; break;
	case 3: break;
	default: continue;
	}
	lon = -lon;    /* CC lib expects negative in the west, which is
			  reverse from what G_scan_easting() returns */

	if(CC_u2ll_north (north) < 0)
	{
	    error (n, buf, zone<0 ? "too far south" : "too far north");
	    continue;
	}
	if(CC_u2ll (east, &lat, &lon) < 0)
	{
	    error (n, buf, "too far from center of zone");
	    continue;
	}
	lat /= 3600.0; /* convert arc seconds to degrees */
	lon /= 3600.0; /* convert arc seconds to degrees */
	lon = -lon;    /* CC lib expects negative in the west, which is
			  reverse from what G_scan_easting() returns */

	G_format_easting (lon, ebuf, PROJECTION_LL);
	G_format_northing (lat, nbuf, PROJECTION_LL);
	printf ("%s %s %s\n", ebuf, nbuf, label);
	if (isatty(0) && !isatty(1))
	    fprintf (stderr, "%s %s %s\n", ebuf, nbuf, label);
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
