/************************************************************
* function
*    convert from latitude/longitude to geocentric
*
* usage
*    ll2gc lat=dd.mm.ss{n|s} lon=dd.mm.ss{e|w} s=spheroid [h=height]
*
*    if the height is missing h=0 is assumed
*          it will computed from the longitude
****************************************************************/
#include "gis.h"
#include "CC.h"
#include "local_proto.h"

int main(int argc, char *argv[])
{
	struct GModule *module;
    struct
    {
	struct Option *lat, *lon, *s, *h;
    } parm;
    double x,y,z,lat,lon,h;
    double a,e;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Converts geographic coordinates to geocentric coordinates.";

    parm.lat = G_define_option();
    parm.lat->key = "lat";
    parm.lat->key_desc = G_lat_format_string();
    parm.lat->type = TYPE_STRING;
    parm.lat->required = YES;
    parm.lat->description = "latitude";

    parm.lon = G_define_option();
    parm.lon->key = "lon";
    parm.lon->key_desc = G_lon_format_string();
    parm.lon->type = TYPE_STRING;
    parm.lon->required = YES;
    parm.lon->description = "longitude";

    parm.s  = G_define_option();
    parm.s->key = "s";
    parm.s->type = TYPE_STRING;
    parm.s->required = YES;
    parm.s->options = spheroid_list();
    parm.s->description = "spheroid";

    parm.h  = G_define_option();
    parm.h->key = "h";
    parm.h->type = TYPE_DOUBLE;
    parm.h->required = NO;
    parm.h->description = "height above earth in meters";
    parm.h->answer = "0.0";

    if (G_parser(argc,argv))
	exit(1);

    if (!G_scan_northing (parm.lat->answer, &lat, PROJECTION_LL))
    {
	fprintf (stderr, "%s=%s - illegal latitude\n", parm.lat->key, parm.lat->answer);
	G_usage();
	exit(1);
    }

    if (!G_scan_easting (parm.lon->answer, &lon, PROJECTION_LL))
    {
	fprintf (stderr, "%s=%s - illegal longitude\n", parm.lon->key, parm.lon->answer);
	G_usage();
	exit(1);
    }

    if(!G_get_ellipsoid_by_name(parm.s->answer, &a, &e))
    {
	fprintf (stderr, "%s=%s - unknown spheroid\n", parm.s->key, parm.s->answer);
	G_usage();
	exit(1);
    }

    sscanf (parm.h->answer, "%lf", &h);

/* convert to arc-seconds */
    lat *=3600;
    lon *= -3600;
    CC_ll2geo (a, e, lat, lon, h, &x, &y, &z);
    fprintf (stdout,"\nx=%f\ny=%f\nz=%f\n", x, y, z);

    exit(0);
}
