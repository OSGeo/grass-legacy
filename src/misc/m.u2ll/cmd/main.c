/************************************************************
* name
*    m.u2ll
*
* function
*    convert from utm to latitude/longitude
*
*
* coordinates are read from input, format:
*
*       east northing anything
*
* output for each input line is:
*
*       lon lat anything
*
*  lon format is ddd:mm:ss{E|W}
*  lat format is dd:mm:ss{N|S}
*
* Note:
*   This program attempt to preserve as much of the input
*   in its orginal form as possible, replacing east north with lon lat.
*   This is accomplished by looking at the first 2 words in the input
*   line. If these two words both start with a digit, then this line
*   is assumed to contain a east and a north to be converted. Other
*   lines are simply copied to the output.
*
*   To deal with possible errors, two flags are provided
*     -o   other lines which do not have east north should
*          be identified (printed to stderr)
*     -w   east north lines in error should NOT be printed to stderr.
*
*   Other flags
*     -d  output format in decimal degrees
*     -s  zone on command line is southern hemisphere
*     -r  east,north is reversed - comes in as north,east
****************************************************************/

#include <unistd.h>
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
	struct Flag *s, *d, *o, *w, *r;
    } flag;
    int n;
    int warning_other;
    int warning_utm;
    int reversed;
    char *spheroid_list();
    char *error;
    double north,east,lat,lon;
    double a,e;
    int proj_format;
    int zone ;
    char buf[1024];
    char ebuf[256], nbuf[256], label[512];
    char b1[100], b2[100];

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Converts Universal Transverse Mercator (UTM) coordinates "
		"to geographic (latitude/longitude) coordinates.";

    parm.spheroid = G_define_option();
    parm.spheroid->key = "spheroid";
    parm.spheroid->description = "reference spheroid (ellipsoid)";
    parm.spheroid->type = TYPE_STRING;
    parm.spheroid->required = YES;
    parm.spheroid->options = spheroid_list();

    parm.zone = G_define_option();
    parm.zone->key = "zone";
    parm.zone->description = "utm zone";
    parm.zone->type = TYPE_INTEGER;
    parm.zone->required = YES;
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

    flag.s = G_define_flag();
    flag.s->key = 's';
    flag.s->description = "Specified zone is in the southern hemisphere";


    flag.r = G_define_flag();
    flag.r->key = 'r';
    flag.r->description = "Input is reversed: north,east";

    flag.w = G_define_flag();
    flag.w->key = 'w';
    flag.w->description = "Do not flag invalid east,north input lines as errors";

    flag.o = G_define_flag();
    flag.o->key = 'o';
    flag.o->description = "Flag other input lines as errors";

    flag.d = G_define_flag();
    flag.d->key = 'd';
    flag.d->description = "Output in decimal degrees";

    if (G_parser(argc,argv))
	exit(1);

    warning_other = flag.o->answer;
    warning_utm   = !flag.w->answer;
    reversed      = flag.r->answer;
    proj_format   = flag.d->answer ? -1 : PROJECTION_LL ;

    if (parm.zone->answer)
    {
	sscanf (parm.zone->answer, "%d", &zone);
	if (flag.s->answer) zone = -zone;
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
	if (reversed)
	    fprintf (stderr, "Enter northing easting, one coordinate pair per line\n");
	else
	    fprintf (stderr, "Enter easting northing, one coordinate pair per line\n");
	fprintf (stderr, "Enter the word <end> when done\n");
    }
    for (n=1; input(b1,reversed?nbuf:ebuf,b2,reversed?ebuf:nbuf,label); n++)
    {
	if (!isnumber(ebuf) || !isnumber(nbuf))
	{
	    sprintf (buf, "%s%s%s%s%s",
		b1, reversed?nbuf:ebuf, b2, reversed?ebuf:nbuf, label);
	    output (buf);
	    if (warning_other)
		warning (buf, n, "ignored");
	}
	else if (!G_scan_easting (ebuf, &east, PROJECTION_UTM)
	     ||  !G_scan_northing (nbuf, &north, PROJECTION_UTM))
	{
	    sprintf (buf, "%s%s%s%s%s",
		b1, reversed?nbuf:ebuf, b2, reversed?ebuf:nbuf, label);
	    output (buf);
	    if (warning_utm)
		warning (buf, n, "invalid coordinate(s)");
	}
	else
	{
	    error = NULL;
	    if(CC_u2ll_north (north) < 0)
	    {
		error = zone<0 ? "too far south" : "too far north";
	    }
	    else if(CC_u2ll (east, &lat, &lon) < 0)
	    {
		error = "too far from center of zone";
	    }

	    if (error)
	    {
		sprintf (buf, "%s-1%s-1%s",b1,b2,label);
		output (buf);
		if (warning_utm)
		{
		    sprintf (buf, "%s%s%s%s%s",
			b1, reversed?nbuf:ebuf, b2, reversed?ebuf:nbuf, label);
		    warning (buf, n, error);
		}
	    }
	    else
	    {
		lat /= 3600.0; /* convert arc seconds to degrees */
		lon /= 3600.0; /* convert arc seconds to degrees */
		lon = -lon;    /* CC lib expects negative in the west, which is
				  reverse from what G_scan_easting() returns */

		G_format_easting (lon, ebuf, proj_format);
		G_format_northing (lat, nbuf, proj_format);
		sprintf (buf, "%s%s%s%s%s",
		    b1, reversed?nbuf:ebuf, b2, reversed?ebuf:nbuf, label);
		output(buf);
	    }
	}
    }
    exit(0);
}
