/************************************************************
* function
*    convert from geocentric to latitude/longitude
*
* usage
*    m.gc2ll x=# y=# z=# s=spheroid
****************************************************************/
#include "gis.h"
#include "CC.h"
#include "local_proto.h"

int main(int argc, char *argv[])
{
	struct GModule *module;
    struct
    {
	struct Option *x, *y, *z, *s, *thresh, *iterations;
    } parm;
    double x,y,z,lat,lon,h;
    double a,e;
    double thresh;
    int i, iterations;
    char buf[300];

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Converts geocentric to geographic coordinates.";

    parm.x = G_define_option();
    parm.x->key = "x";
    parm.x->type = TYPE_DOUBLE;
    parm.x->required = YES;
    parm.x->description = "x coordinate";

    parm.y = G_define_option();
    parm.y->key = "y";
    parm.y->type = TYPE_DOUBLE;
    parm.y->required = YES;
    parm.y->description = "y coordinate";

    parm.z = G_define_option();
    parm.z->key = "z";
    parm.z->type = TYPE_DOUBLE;
    parm.z->required = YES;
    parm.z->description = "z coordinate";

    parm.s = G_define_option();
    parm.s->key = "s";
    parm.s->type = TYPE_STRING;
    parm.s->required = YES;
    parm.s->description = "spheroid";
    parm.s->options = spheroid_list();

    parm.thresh = G_define_option();
    parm.thresh->key = "thresh";
    parm.thresh->type = TYPE_DOUBLE;
    parm.thresh->required = NO;
    parm.thresh->description = "conversion threshold";
    parm.thresh->answer = "1e-6";

    parm.iterations = G_define_option();
    parm.iterations->key = "iterations";
    parm.iterations->type = TYPE_INTEGER;
    parm.iterations->required = NO;
    parm.iterations->description = "max iterations";
    parm.iterations->answer = "30";

    if (G_parser(argc,argv))
	exit(1);

    sscanf (parm.x->answer, "%lf", &x);
    sscanf (parm.y->answer, "%lf", &y);
    sscanf (parm.z->answer, "%lf", &z);
    if(!G_get_ellipsoid_by_name(parm.s->answer, &a, &e))
    {
	fprintf (stderr, "%s=%s - unknown spheroid\n", parm.s->key, parm.s->answer);
	G_usage();
	exit(1);
    }
    sscanf (parm.thresh->answer, "%lf", &thresh);
    if (thresh <= 0.0)
    {
	fprintf (stderr, "%s=%s - must be a positive value\n",
		parm.thresh->key, parm.thresh->answer);
	G_usage();
	exit(1);
    }
    sscanf (parm.iterations->answer, "%d", &iterations);
    if (iterations <= 0)
    {
	fprintf (stderr, "%s=%s - must be a positive value\n",
		parm.iterations->key, parm.iterations->answer);
	G_usage();
	exit(1);
    }

    i = CC_geo2ll (a, e, x, y, z, &lat, &lon, &h, iterations, thresh);
    fprintf (stdout,"\n");

 /* convert to degrees */
    lat /= 3600;
    lon /= -3600;
    G_format_northing (lat, buf, PROJECTION_LL);
    fprintf (stdout,"lat=%s\n", buf);
    G_format_easting (lon, buf, PROJECTION_LL);
    fprintf (stdout,"lon=%s\n", buf);
    fprintf (stdout,"h=%f\n", h);
    if (i == 0)
	fprintf (stderr,"\nnote: did not converge in %d iterations with thresh=%g\n",
		iterations, thresh);

    exit(i);
}
