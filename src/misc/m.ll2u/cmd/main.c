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
* coordinates are read from input, format:
*         lon lat anything
*
* output for each input line is:
*         east west [zone] anything
*   [zone] is suppressed if -z specified
*
*  lon format is ddd:mm:ss{E|W}
*  lat format is dd:mm:ss{N|S}
*
*
* Note:
*   This program attempt to preserve as much of the input
*   in its orginal form as possible, replacing lon lat with east north.
*   This is accomplished by looking at the first 2 words in the input
*   line. If these two words both start with a digit, then this line
*   is assumed to contain a lon and a lat to be converted. Other
*   lines are simply copied to the output.
*
*   To deal with possible errors, two flags are provided
*     -o   other lines which do not have lon lat should
*          be identified (printed to stderr)
*     -w   lon lat lines in error should NOT be printed to stderr.
****************************************************************/
#include <unistd.h>
#include <string.h>
#include "gis.h"
#include "CC.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	struct GModule *module;
    struct
    {
	struct Option *spheroid, *zone, *input, *output;
    } parm;
    struct
    {
	struct Flag *o, *w, *z, *r;
    } flag;
    int n;
    char *error;
    double north,east,lat,lon;
    double a,e;
    int zone,z ;
    int want_zone;
    int reversed;
    int warning_other;
    int warning_ll;
    char ebuf[256], nbuf[256], label[512];
    char b1[100], b2[100];
    char zbuf[100];
    char buf[1024];

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Converts geographic coordinates to Universal Transverse "
		"Mercator (UTM) coordinates.";

    parm.spheroid = G_define_option();
    parm.spheroid->key = "spheroid";
    parm.spheroid->description = "reference spheroid (ellipsoid)";
    parm.spheroid->type = TYPE_STRING;
    parm.spheroid->required = YES;
    parm.spheroid->options = spheroid_list();

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

    flag.r = G_define_flag();
    flag.r->key = 'r';
    flag.r->description = "Input is reversed: lat,lon";

    flag.w = G_define_flag();
    flag.w->key = 'w';
    flag.w->description = "Do not flag invalid lon,lat input lines as errors";

    flag.o = G_define_flag();
    flag.o->key = 'o';
    flag.o->description = "Flag other input lines as errors";

    flag.z = G_define_flag();
    flag.z->key = 'z';
    flag.z->description = "Suppress printing the utm zone";

    if (G_parser(argc,argv))
	exit(1);
    
    warning_other = flag.o->answer;
    warning_ll    = !flag.w->answer;
    want_zone     = !flag.z->answer;
    reversed      = flag.r->answer;

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

    if (isatty(0))
    {
	if (reversed)
	{
	    fprintf (stderr,"Enter lat lon, one coordinate pair per line, in the format\n");
	    fprintf (stderr," %s %s\n", G_lat_format_string(), G_lon_format_string());
	}
	else
	{
	    fprintf (stderr,"Enter lon lat, one coordinate pair per line, in the format\n");
	    fprintf (stderr," %s %s\n", G_lon_format_string(), G_lat_format_string());
	}
	fprintf (stderr,"Enter the word <end> when done\n");
    }
    for (n=1; input(b1,reversed?nbuf:ebuf,b2,reversed?ebuf:nbuf,label); n++)
    {
	if (!might_be_number(ebuf) || !might_be_number(nbuf))
	{
	    sprintf (buf, "%s%s%s%s%s",
		b1, reversed?nbuf:ebuf, b2, reversed?ebuf:nbuf, label);
	    output (buf);
	    if (warning_other)
		warning (buf, n, "ignored");
	}
	else if (!G_scan_easting (ebuf, &lon, PROJECTION_LL)
	     ||  !G_scan_northing (nbuf, &lat, PROJECTION_LL))
	{
	    sprintf (buf, "%s%s%s%s%s",
		b1, reversed?nbuf:ebuf, b2, reversed?ebuf:nbuf, label);
	    output (buf);
	    if (warning_ll)
		warning (buf, n, reversed?"invalid lat lon":"invalid lon lat");
	}
	else
	{
	    lat *= 3600.0; /* convert to arc seconds for CC library */
	    lon *= 3600.0; /* convert to arc seconds for CC library */
	    lon = -lon;    /* CC lib expects negative in the west, which is
			      reverse from what G_scan_easting() returns */

	    z = zone; /* parser ensures that zone is never negative */
	    if (z != 0 && lat < 0)
		z = -z;

	    error = NULL;
	    switch (CC_ll2u (lat, lon, &east, &north, &z))
	    {
	    case -1:
		error = lat < 0 ? "too far south" : "too far north";
	    case -2:
		error = "too far from center of utm zone";
	    }

	    if (error)
	    {
		sprintf (buf, "%s-1%s-1%s",b1,b2,label);
		output (buf);
		if (warning_ll)
		{
		    sprintf (buf, "%s%s%s%s%s",
			b1, reversed?nbuf:ebuf, b2, reversed?ebuf:nbuf, label);
		    warning (buf, n, error);
		}
	    }
	    else
	    {
		G_format_easting (east, ebuf, PROJECTION_UTM);
		G_format_northing (north, nbuf, PROJECTION_UTM);
		sprintf (buf, "%s%s%s%s",
		    b1, reversed?nbuf:ebuf, b2, reversed?ebuf:nbuf);
		if (want_zone)
		{
		    sprintf (zbuf, " %d", z);
		    strcat (buf, zbuf);
		}
		strcat (buf, label);
		output(buf);
	    }
	}
    }
    exit(0);
}
