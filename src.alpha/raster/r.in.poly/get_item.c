#include "gis.h"
get_item(fd, type, cat, x, y, count, labels)
    FILE *fd;
    int *type;
    long *cat;
    double **x, **y;
    int *count;
    struct Categories *labels;
{
    static double *X = NULL;
    static double *Y = NULL;
    static int nalloc = 0;
    char buf[1024];
    char lbl[1024];
    char east[256], north[256];
    double e,n;
    long offset;

    *cat = 0;
    *count = 0;
    *type = 0;
    while (fgets(buf, sizeof buf, fd))
    {
	G_strip(buf);
	if (*buf == 'A' || *buf == 'a')
	{
	    *type = 'A';
	    break;
	}
	if (*buf == 'L' || *buf == 'l')
	{
	    *type = 'L';
	    break;
	}
    }
    if (*type == 0) return 0;

    while(1)
    {
	offset = ftell (fd);
	if (!fgets(buf, sizeof buf, fd))
	    break;
	G_strip(buf);
	if (*buf == 'A' || *buf == 'a' || *buf == 'L' || *buf == 'l')
	{
	    fseek (fd, offset, 0);
	    break;
	}
	if (*buf == '=')
	{
	    if (sscanf (buf+1, "%ld", cat) != 1)
		continue;
	    if (sscanf (buf+1, "%ld%[^\n]", cat, lbl) == 2)
	    {
		G_strip(lbl);
		G_set_cat ((CELL)*cat, lbl, labels);
	    }
	    continue;
	}
	if (sscanf (buf, "%s %s", east, north) != 2)
	    continue;
	if (!G_scan_northing(north, &n, G_projection()))
	    continue;
	if (!G_scan_easting(east, &e, G_projection()))
	    continue;

	if (*count >= nalloc)
	{
	    nalloc += 32;
	    X = (double *) G_realloc (X, nalloc * sizeof (double));
	    Y = (double *) G_realloc (Y, nalloc * sizeof (double));
	}
	X[*count] = e;
	Y[*count] = n;
	(*count)++ ;
    }
    *x = X;
    *y = Y;
    return 1;
}
