/***************************************************************
G_lat_format (lat, buf)
    double lat;
    char *buf;

G_lon_format (lon, buf)
    double lon;
    char *buf;

G_llres_format (res, buf)
    double res;
    char *buf;

  formats lat (latitude in degrees), or lon (longitude in degrees)
  into buf as dd:mm:ssH, where H (hemishpere) is
      N for nothern hemishpere, S for southern,
      W for western hemishpere, E for eastern
      none for resolution
  (lat > 0 is northern, lat < 0 is southern)
  (lon > 0 is eastern,  lon < 0 is western)

Note: lat should be in the range -90 to 90s, but
	  the range is NOT checked by G_lat_format().
      lon can be anything, but
	  values outside [-180,180] are moved into this range 
          by adding (or subtracting) 360.

NOTE: These routines are used by G_format_northing(), G_format_easting(), and
      G_format_resolution(). Those routines are intended to provide
      a general interface to window values and should be used instead of
      these projection specific routines. In other words, these routines
      are for the library only, programmers shouldn't use them.
***************************************************************/
G_lat_format (lat, buf)
    double lat;
    char *buf;
{
    int d,m;
    char h;
    double s;

    G_lat_parts (lat, &d, &m, &s, &h);
    format (buf, d,m,s,h);
}

G_lon_format (lon, buf)
    double lon;
    char *buf;
{
    int d,m;
    char h;
    double s;

    G_lon_parts (lon, &d, &m, &s, &h);
    format (buf, d,m,s,h);
}

G_llres_format (res, buf)
    double res;
    char *buf;
{
    int d,m;
    char h;
    double s;

    G_lat_parts (res, &d, &m, &s, &h);
    h = 0;
    format (buf, d,m,s,h);
}


static
format (buf, d,m,s,h)
    char *buf;
    int d,m;
    double s;
    char h;
{
    char temp[50];
    double ss;
    int x;

    sprintf (temp, "%lf", s);
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

    sprintf (temp, "%lf", ss);
    x = strlen (temp);
    while (x-- > 0)
	if (temp[x] == '0' || temp[x] == '.')
	    temp[x] = 0;
	else
	    break;

    if (*temp)
    {
	if (temp[0] == '.')
	    sprintf (buf, "%d:%02d:00%s%c", d, m, temp, h);
	else if (temp[1] == '.' || temp[1] == 0)
	    sprintf (buf, "%d:%02d:0%s%c", d, m, temp, h);
	else
	    sprintf (buf, "%d:%02d:%s%c", d, m, temp, h);
    }
    else if (m > 0)
	sprintf (buf, "%d:%02d%c", d, m, h);
    else if (d > 0)
	sprintf (buf, "%d%c", d, h);
    else
	sprintf (buf, "0");
}

G_lat_parts (lat, d, m, s, h)
    double lat;     /* lat in degrees to be split into parts */
    int *d, *m;     /* degrees, minutes */
    double *s;      /* seconds */
    char *h;        /* hemisphere */
{
    if (lat < 0)
    {
	*h = 'S' ;
	lat = -lat;
    }
    else
	*h = 'N' ;

    ll_parts (lat, d, m, s);
}

G_lon_parts (lon, d, m, s, h)
    double lon;	    /* lon in degrees to be split into parts */
    int *d, *m;     /* degrees, minutes */
    double *s;      /* seconds */
    char *h;        /* hemisphere */
{
    while (lon > 180.0)
	lon -= 360.0;
    while (lon < -180.0)
	lon += 360.0;

    if (lon < 0)
    {
	*h = 'W' ;
	lon = -lon;
    }
    else
	*h = 'E' ;

    ll_parts (lon, d, m, s);
}

static
ll_parts (ll, d, m, s)
    double ll;	/* ll in degrees to be split into parts */
    int *d, *m; /* degrees, minutes */
    double *s;  /* seconds */
{
    *d = ll ;
    *m = (ll - *d) * 60;
    if (*m < 0) *m = 0;
    *s = ((ll - *d) * 60 - *m) * 60 ;
    if (*s < 0) *s = 0;
}
