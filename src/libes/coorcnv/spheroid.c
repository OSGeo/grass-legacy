static struct
{
    char *name;
    double a;		/* semi-major axis */
    double e;		/* eccentricity squared */
}
spheroid[] =
{
    "australian",    6378160.0,   0.0066945419,
    "bessel",        6377739.155, 0.0066743722,
    "clark66",       6378206.4,   0.006768658,
    "clark80",       6378249.145, 0.0068035113,
    "everest",       6377276.345, 0.0066378466,
    "international", 6378388.0,   0.00672267,
    "wgs72",         6378135.0,   0.006694317778,
    "wgs84",         6378137.0,   0.00669437999
};

CC_get_spheroid (name, a, e)
    char *name;
    double *a, *e;
{
    int n;

    n = sizeof(spheroid)/sizeof(spheroid[0]);

    while (n--)
	if (equal (name, spheroid[n].name))
	{
	    *a = spheroid[n].a;
	    *e = spheroid[n].e;
	    return 1;
	}

    return 0 ;
}

char *
CC_spheroid_name (n)
{
    if (n < 0 || n >= sizeof(spheroid)/sizeof(spheroid[0]))
	return 0;

    return spheroid[n].name;
}

static
equal (a, b)
    char *a, *b;
{
    char lcase();

    while (*a)
	if (lcase (*a++) != lcase (*b++))
	    return 0;
    
    return *b == 0;
}

static char
lcase (c)
    char c;
{
    if (c >= 'A' && c <= 'Z')
	c += 'a' - 'A';
    return c;
}
