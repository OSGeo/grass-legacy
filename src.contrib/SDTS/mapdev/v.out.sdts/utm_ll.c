#include "gis.h"

char *
spheroid_list()
{
    int len,n;
    char *list, *G_ellipsoid_name();
    char *name;

    len=0;
    for (n=0; name = G_ellipsoid_name(n); n++)
	len += strlen(name)+1;
    list = G_malloc(len);
    for (n=0; name = G_ellipsoid_name(n); n++)
    {
	if (n) strcat (list, ",");
	else *list = 0;
	strcat (list, name);
    }
    return list;
}


setup_ll_conv (zone_answer, spheroid_answer, spheroid_key, s_flag)
   char *zone_answer, *spheroid_answer, *spheroid_key;
   char s_flag;
{ 
	int zone;
	double a, e;

	if (zone_answer)
	{
		sscanf (zone_answer, "%d", &zone);
		if (s_flag) zone = -zone;
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

	if (!spheroid_answer)
	{
		fprintf (stderr, "You must specify spheroid for lat/lon conversion.\nEXITING.\n");
        G_usage();
        exit(1);
    }

	if(!G_get_ellipsoid_by_name(spheroid_answer, &a, &e))
	{
		fprintf (stderr, "%s=%s - unknown spheroid\n",
		spheroid_key, spheroid_answer);
		G_usage();
		exit(1);
	}

	CC_u2ll_spheroid_parameters(a,e);
	CC_u2ll_zone (zone);

	return (zone);
}
#define LL_FACTOR   1000000 /*thus SDTS scale factor = .000001*/

utm_to_ll (east, north, int_lat, int_lon, zone )
    double east, north;
	long *int_lat, *int_lon;
	int zone;
{

	char *error;
	double lat, lon;
#if 0
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
	   fprintf (stderr, "%s\n", error);

/*fprintf (stderr, "north = %lfeast = %lf lat = %lf lon = %lf\n", 
		 north, east, lat, lon); */

	lat /= 3600.0; /* convert arc seconds to degrees */
	lon /= 3600.0; /* convert arc seconds to degrees */

	lon = - (lon);

	*int_lat = (int) (lat * LL_FACTOR);
	*int_lon = (int) (lon * LL_FACTOR);
#endif
	*int_lat =  north * 10;
	*int_lon =  east* 10;
}
