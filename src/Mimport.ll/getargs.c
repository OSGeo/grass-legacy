/* %W% %G% */

#include "geo.h"
getargs (argc, argv, geo, infile, outfile)
    char *argv[];
    struct GEO *geo;
    char *infile;
    char *outfile;
{
    int i;
    int k;
    double x;
    int corner;
    int have_spheroid;
    double lat, lon;
    int hemi1, hemi2;
    char temp[100], temp2[100];
    char temp3[5];
    char *CC_spheroid_name();

    corner = 0;
    have_spheroid = 0;
    geo->nrows = geo->ncols = 0;
    geo->lat_res = geo->lon_res = 0.0;
    geo->bpc = 0;
    geo->sflag = 1;

    *infile = *outfile = 0;

    for (i = 1; i < argc; i++)
    {
	if (sscanf (argv[i], "if=%s", temp) == 1)
	{
	    if (*infile) return 0;
	    strcpy (infile, temp);
	    continue;
	}

	if (sscanf (argv[i], "of=%s", temp) == 1)
	{
	    if (*outfile) return 0;
	    strcpy (outfile, temp);
	    continue;
	}

	if (sscanf (argv[i], "rows=%d", &k) == 1)
	{
		if (geo->nrows) return 0;
		geo->nrows = k;
		continue;
	}

	if (sscanf (argv[i], "cols=%d", &k) == 1)
	{
		if (geo->ncols) return 0;
		geo->ncols = k;
		continue;
	}

	if(sscanf (argv[i], "bpc=%d%1s", &k, temp) == 2)
	{
	    if (geo->bpc) return 0;
	    if (*temp != 'u') return 0;

	    geo->bpc = k;
	    geo->sflag = 0;
	    continue;
	}
	if(sscanf (argv[i], "bpc=%d", &k) == 1)
	{
	    if (geo->bpc) return 0;
	    geo->bpc = k;
	    continue;
	}
	if (sscanf (argv[i], "latres=%lf", &x) == 1)
	{
		if (geo->lat_res) return 0;
		geo->lat_res = x;
		continue;
	}

	if (sscanf (argv[i], "lonres=%lf", &x) == 1)
	{
		if (geo->lon_res) return 0;
		geo->lon_res = x;
		continue;
	}

	if (sscanf (argv[i], "s=a=%lf,e=%lf", &geo->a, &geo->e) == 2
	||  sscanf (argv[i], "s=e=%lf,a=%lf", &geo->e, &geo->a) == 2)
	{
	    if (have_spheroid)
		return 0;
	    if (geo->a <= 0.0 || geo->e < 0.0 || geo->e > 1.0)
	    {
		fprintf (stderr, "%s - illegal spheroid values\n", &argv[i][2]);
		return 0;
	    }
	    have_spheroid = 1;
	    continue;
	}
	if (sscanf (argv[i], "s=%s", temp) == 1)
	{
	    if (have_spheroid)
		return 0;
	    if(!CC_get_spheroid (temp, &geo->a, &geo->e))
	    {
		fprintf (stderr, "%s - unknown spheroid. ", temp);
		fprintf (stderr, "known spheroids are\n");
		for (i = 0; CC_spheroid_name(i); i++)
		{
		    if (i%3 == 0) fprintf (stderr, "  ");
		    fprintf (stderr, "%-20s ", CC_spheroid_name(i));
		    if (i%3 == 2) fprintf (stderr, "\n");
		}
		fprintf (stderr, "\n");
		return 0;
	    }
	    have_spheroid = 1;
	    continue;
	}

	if (sscanf (argv[i], "%2c=%[^,],%s", temp3, temp, temp2) == 3)
	{
	    temp3[2] = 0;

	    if (corner) return 0;
	    if (strcmp (temp3, "nw") == 0)
		corner = 1;
	    else if (strcmp (temp3, "ne") == 0)
		corner = 2;
	    else if (strcmp (temp3, "se") == 0)
		corner = 3;
	    else if (strcmp (temp3, "sw") == 0)
		corner = 4;
	    else
		return 0;

	    hemi1 = scan_geo (temp, &x);
	    switch (hemi1)
	    {
		case 1: lon = x; break;
		case 2: lat = x; break;
		default: return 0;
	    }
	    hemi2 = scan_geo (temp2, &x);
	    if (hemi1 == hemi2) return 0;
	    switch (hemi2)
	    {
		case 1: lon = x; break;
		case 2: lat = x; break;
		default: return 0;
	    }

	    continue;
	}
	return 0;
    }

    if (geo->lat_res <= 0.0 || geo->lon_res <= 0.0) return 0;
    if (geo->nrows <= 0 || geo->ncols <= 0) return 0;
    if (!have_spheroid) return 0;
    if (geo->bpc <= 0) return 0;

    switch (corner)
    {
    case 1: /* nw */
		geo->lat = lat;
		geo->lon = lon;
		break;
    case 2: /* ne */
		geo->lat = lat;
		geo->lon = lon + geo->ncols * geo->lon_res;
		break;
    case 3: /* se */
		geo->lat = lat + geo->nrows * geo->lat_res;
		geo->lon = lon + geo->ncols * geo->lon_res;
		break;
    case 4: /* sw */
		geo->lat = lat + geo->nrows * geo->lat_res;
		geo->lon = lon;
		break;
    default:
		return 0;
    }

    if (*infile == 0) return 0;
    if (*outfile == 0) return 0;

    return 1;
}
