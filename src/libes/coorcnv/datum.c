static struct
{
    char *name, *description;
    double dx,dy,dz;
}
datum[] =
{
    "wgs72", "World Geodetic System 1972",         0.0,    0.0,    0.0,
    "na27",  "North American 1927",              -22.0,  157.0,  176.0,
    "a&c",   "Alaska and Canada",                 -9.0,  139.0,  173.0,
    "eur",   "European",                         -84.0, -103.0, -127.0,
    "tokyo", "Tokyo",                           -140.0,  516.0,  673.0,
    "aus",   "Australian Geodetic",             -122.0,  -41.0,  146.0,
    "osgb",  "Ordnance Survey of Great Britain", 368.0, -120.0,  425.0,
    "sa69",  "South American 1969",              -77.0,    3.0,  -45.0
};

CC_datum_shift (name, dx, dy, dz)
    char *name;
    double *dx, *dy, *dz;
{
    int n;

    n = datum_n (name);
    if (n < 0) return 0;

    *dx = datum[n].dx;
    *dy = datum[n].dy;
    *dz = datum[n].dz;
    return 1;
}

char *
CC_datum_name (n)
{
    if (n < 0 || n >= sizeof (datum) / sizeof (datum[0]))
	return 0;
    return datum[n].name;
}

char *
CC_datum_description (n)
{
    if (n < 0 || n >= sizeof (datum) / sizeof (datum[0]))
	return 0;
    return datum[n].description;
}

static datum_n (name)
    char *name;
{
    int n;

    n = sizeof (datum) / sizeof (datum[0]);
    while (n-- > 0)
	if (equal (name, datum[n].name))
	    return n;
    return -1;
}
