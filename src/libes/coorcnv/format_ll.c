/***************************************************************
CC_lat_format (lat, buf)
    double lat;
    char *buf;

CC_lon_format (lon, buf)
    double lon;
    char *buf;

  formats lat (latitude in seconds), or lon (longitude in seconds)
  into buf as dd.mm.ssH, where H (hemishpere) is
  N for nothern hemishpere, S for southern,
  W for western hemishpere, E for eastern
  (lat > 0 is northern, lat < 0 is southern)
  (lon > 0 is western, lon < 0 is eastern)
***************************************************************/
#include <stdio.h>
#include <string.h>
#include "CC.h"

static int format(char *,int,int,double,char);
static int ll_parts(double,int *,int *,double *);

int CC_lat_format (double lat, char *buf)
{
    int d,m;
    char h;
    double s;

    CC_lat_parts (lat, &d, &m, &s, &h);
    format (buf, d,m,s,h);

    return 0;
}

int CC_lon_format (double lon, char *buf)
{
    int d,m;
    char h;
    double s;

    CC_lon_parts (lon, &d, &m, &s, &h);
    format (buf, d,m,s,h);

    return 0;
}

static int format(char *buf,int d,int m,double s,char h)
{
    char temp[50];
    double ss;
    int x;

    sprintf (temp, "%f", s);
    sscanf (temp, "%lf", &ss);
    if (ss >= 60)
    {
	ss = 0;	/* force it to zero */
	if (++m >= 60)
	{
	    m = 0;
	    d++;
	}
    }

    sprintf (temp, "%f", ss);
    x = strlen (temp);
    while (x-- > 0)
	if (temp[x] == '0' || temp[x] == '.')
	    temp[x] = 0;
	else
	    break;

    if (*temp)
    {
	if (temp[0] == '.')
	    sprintf (buf, "%d.%02d.00%s%c", d, m, temp, h);
	else if (temp[1] == '.')
	    sprintf (buf, "%d.%02d.0%s%c", d, m, temp, h);
	else
	    sprintf (buf, "%d.%02d.%s%c", d, m, temp, h);
    }
    else if (m > 0)
	sprintf (buf, "%d.%02d%c", d, m, h);
    else
	sprintf (buf, "%d%c", d, h);

    return 0;
}

int CC_lat_parts (
    double lat,     /* lat to be split into parts */
    int *d,
    int *m,     /* degrees, minutes */
    double *s,      /* seconds */
    char *h        /* hemisphere */
)
{
    if (lat < 0)
    {
	*h = 'S' ;
	lat = -lat;
    }
    else
	*h = 'N' ;
    
    ll_parts (lat, d, m, s);

    return 0;
}

int CC_lon_parts (
    double lon,	    /* lon to be split into parts */
    int *d,
    int *m,     /* degrees, minutes */
    double *s,      /* seconds */
    char *h        /* hemisphere */
)
{
    if (lon < 0)
    {
	*h = 'E' ;
	lon = -lon;
    }
    else
	*h = 'W' ;
    
    ll_parts (lon, d, m, s);

    return 0;
}

static int ll_parts (
    double ll,	/* ll to be split into parts */
    int *d,int *m, /* degrees, minutes */
    double *s)  /* seconds */
{
    long x;

    x = ll;

    *d = x / 3600 ;
    *m = (x % 3600) / 60;

    *s = ll - *d * 3600 - *m * 60 ;

    return 0;
}
