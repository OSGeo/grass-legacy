/************************************************************
* name
*    m.ll2db
*
* function
*    convert between latitude/longitude and GRASS database units
*
* usage
*    ll2db -rwoi [input=file] [output=file]
*
* coordinates are read from input, format:
*         lon lat anything
*
* output for each input line is:
*         east west anything
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
#include "projects.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	struct GModule *module;
    struct
    {
	struct Option *input, *output;
    } parm;
    struct
    {
	struct Flag *o, *w, *r, *i, *d;
    } flag;
    int n;
    char *error;
    double north,east,lat,lon;
    int reversed, inv, proj, proj_inv;
    int warning_other;
    int warning_ll;
    int dec_deg;
    char ebuf[256], nbuf[256], label[512];
    char b1[100], b2[100];
    char buf[1024];
    struct pj_info info_in;
    struct pj_info info_out;
    char parms_in[512];
    struct Key_Value *out_proj_keys, *out_unit_keys;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Converts GRASS Database coordinates "
		"to / from Latitude Longitude coordinates ";

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

    flag.i = G_define_flag();
    flag.i->key = 'i';
    flag.i->description = "Inverse projection [Database to Lat. Lon.]";

    flag.d = G_define_flag();
    flag.d->key = 'd';
    flag.d->description = "Output Lat. Lon. as decimal degrees [Inverse]";

    if (G_parser(argc,argv))
	exit(1);
    
    warning_other = flag.o->answer;
    warning_ll    = !flag.w->answer;
    reversed      = flag.r->answer;
    inv		  = flag.i->answer;
    dec_deg       = flag.d->answer;

if (inv) {
    proj_inv 	  = G_projection();
    proj 	  = PROJECTION_LL;
	} else {
    proj          = G_projection();
    proj_inv      = PROJECTION_LL;
	}

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

/* Test Projection Type */
if (G_projection() == PROJECTION_LL) {
fprintf (stderr, "Error - This program cannot be used with a Lat. Long.
projection\nUse m.proj\n");
exit(1);
}

if (G_projection() == PROJECTION_XY) {
fprintf (stderr, "Error - This program cannot be used with an XY 
projection\nUse m.proj\n");
exit(1);
}

/* Out Info */
out_proj_keys = G_get_projinfo();
out_unit_keys = G_get_projunits();
if (pj_get_kv(&info_out,out_proj_keys,out_unit_keys) < 0) {
exit (0);
}

/* In Info */
if( G_find_key_value("ellps", out_proj_keys) != NULL )
    sprintf(parms_in, "proj=ll ellps=%s", 
	    G_find_key_value("ellps", out_proj_keys) );
else
    sprintf(parms_in, "proj=ll ellps=wgs84");
pj_get_string(&info_in, parms_in);

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
	else if (!G_scan_easting (ebuf, &lon, proj_inv)
	     ||  !G_scan_northing (nbuf, &lat, proj_inv))
	{
	    sprintf (buf, "%s%s%s%s%s",
		b1, reversed?nbuf:ebuf, b2, reversed?ebuf:nbuf, label);
	    output (buf);
	    if (warning_ll)
		warning (buf, n, reversed?"invalid lat lon":"invalid lon lat");
	}
	else
	{

	    error = NULL;

	if (inv) {
	/* Inverse projection */
	if (pj_do_proj(&lon, &lat, &info_out, &info_in) <0) {
		fprintf(stderr,"Inverse pj_do_proj error\n");
		exit(0);
		}
	} else {
	/* Forward projection */
	 if (pj_do_proj(&lon, &lat, &info_in,&info_out) <0) {
		fprintf(stderr,"Error in pj_do_proj\n");
		exit(0);
		}
	}

		east = lon;
		north = lat;

	if (inv && dec_deg) proj = proj_inv ;

		G_format_easting (east, ebuf, proj);
		G_format_northing (north, nbuf, proj);
		sprintf (buf, "%s%s%s%s",
		    b1, reversed?nbuf:ebuf, b2, reversed?ebuf:nbuf);
		strcat (buf, label);
		output(buf);
	    }
    }
    exit(0);
}
