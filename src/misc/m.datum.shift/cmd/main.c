/************************************************************
* name
*    m.dshift
*
* function
*    convert latitude/longitude from one datum to another
*
* usage
*    m.dshift lat=dd.mm.ss{n|s} lon=dd.mm.ss{e|w} is=input-spheroid
*                os=output-spheroid xshift=# yshift=# zshift=#
*
****************************************************************/
#include "gis.h"

main (argc, argv) char *argv[];
{
    double x,y,z,lat,lon,h;
    double dx,dy,dz;
    double ia,ie;
    double oa,oe;
    char buf[300];
    struct
    {
	struct Option *lat, *lon, *is, *os, *dx, *dy, *dz;
    } parm;
    char *sphlist, *spheroid_list();

    G_gisinit(argv[0]);

    sphlist = spheroid_list();

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

    parm.is  = G_define_option();
    parm.is->key = "is";
    parm.is->type = TYPE_STRING;
    parm.is->required = YES;
    parm.is->options = sphlist;
    parm.is->description = "input spheroid";

    parm.os  = G_define_option();
    parm.os->key = "os";
    parm.os->type = TYPE_STRING;
    parm.os->required = YES;
    parm.os->options = sphlist;
    parm.os->description = "output spheroid";

    parm.dx  = G_define_option();
    parm.dx->key = "xshift";
    parm.dx->type = TYPE_DOUBLE;
    parm.dx->required = YES;
    parm.dx->description = "x shift";

    parm.dy  = G_define_option();
    parm.dy->key = "yshift";
    parm.dy->type = TYPE_DOUBLE;
    parm.dy->required = YES;
    parm.dy->description = "y shift";

    parm.dz  = G_define_option();
    parm.dz->key = "zshift";
    parm.dz->type = TYPE_DOUBLE;
    parm.dz->required = YES;
    parm.dz->description = "z shift";

    if (G_parser(argc,argv))
	exit(1);
    sscanf (parm.dx->answer, "%lf", &dx);
    sscanf (parm.dy->answer, "%lf", &dy);
    sscanf (parm.dz->answer, "%lf", &dz);

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

    if(!G_get_ellipsoid_by_name(parm.is->answer, &ia, &ie))
    {
	fprintf (stderr, "%s=%s - unknown spheroid\n", parm.is->key, parm.is->answer);
	G_usage();
	exit(1);
    }

    if(!G_get_ellipsoid_by_name(parm.os->answer, &oa, &oe))
    {
	fprintf (stderr, "%s=%s - unknown spheroid\n", parm.os->key, parm.os->answer);
	G_usage();
	exit(1);
    }

/* now convert lat and lon to arc seconds for CC library.
 * Also invert sense of longitude
 */
    lat *= 3600;
    lon *= -3600;

    h = 0.0;
    CC_ll2geo (ia, ie, lat, lon, h, &x, &y, &z);
    x += dx;
    y += dy;
    z += dz;
    CC_geo2ll (oa, oe, x, y, z, &lat, &lon, &h, 20, (double) 1.0e-6);

/* convert results back to degrees */
    lat /= 3600;
    lon /= -3600;

    G_format_northing (lat, buf, PROJECTION_LL);
    printf ("lat=%s\n", buf);

    G_format_easting (lon, buf, PROJECTION_LL);
    printf ("lon=%s\n", buf);

    exit(0);
}
