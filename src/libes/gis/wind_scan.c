#include "gis.h"
static int scan_double(char *,double *);

int G_scan_northing ( char *buf, double *northing, int projection)
{
    if (projection == PROJECTION_LL)
    {
	if(G_lat_scan (buf, northing))
	    return 1;
	if (!scan_double (buf, northing))
	    return 0;
	return (*northing <= 90.0 && *northing >= -90.0);
    }
    return scan_double (buf, northing);
}

int G_scan_easting ( char *buf, double *easting, int projection)
{
    if (projection == PROJECTION_LL)
    {
	if (G_lon_scan (buf, easting))
	    return 1;
	if (!scan_double (buf, easting))
	    return 0;
	while (*easting > 180.0)
	    *easting -= 360.0;
	while (*easting < -180.0)
	    *easting += 360.0;
	return 1;
    }
    return scan_double (buf, easting);
}

int G_scan_resolution ( char *buf, double *res,int projection)
{
    if (projection == PROJECTION_LL)
    {
	if(G_llres_scan (buf, res))
		return 1;
    }
    return (scan_double (buf, res) && *res > 0.0);
}

static int scan_double(char *buf, double *value)
{
    char junk[2];

/* use sscanf to convert buf to double
 * make sure value doesn't have other characters after it
 */
    *junk = 0;
    *value = 0.0;
    if(sscanf (buf, "%lf%1s", value, junk) == 1 && *junk == 0)
    {
       while(*buf) buf++;
       buf--;
       if(*buf>='A'&&*buf<='Z')
 	  return 0;
       if(*buf>='a'&&*buf<='z')
 	  return 0;
       return 1;
     }
     return 0;
}
