#include "glob.h"

void
read_point_list (filename, fs)
    char *filename;
    char *fs;
{
    int errors;
    char buf[4096];
    double x, y;
    long offset;
    int nalloc;

    pointlist.fd = NULL;
    pointlist.x = NULL;
    pointlist.y = NULL;
    pointlist.offset = NULL;
    pointlist.reported = NULL;
    pointlist.npoints = 0;
    pointlist.fs = fs;


    pointlist.fd = fopen (filename,"r");
    if (pointlist.fd == NULL)
    {
	perror (filename);
	exit(1);
    }
    if (!be_quiet())
	fprintf (stderr, "Reading input file [%s] ...\n", filename);

    nalloc = 0;
    errors = 0;

    while(1)
    {
	offset = ftell (pointlist.fd);
	if(!readline(pointlist.fd, buf, sizeof buf))
	    break;
	if (is_comment (buf))
	    continue;
	if (!get_xy (buf, &x, &y, fs))
	{
	    if (!errors)
	    {
		fprintf (stderr, "WARNING: input file [%s] has unrecognized lines. Is your fs correct?\n", filename);
		errors = 1;
	    }
	    continue;
	}
	if (have_region() && !point_in_region(x,y))
	    continue;

	if (pointlist.npoints >= nalloc)
	    extend_pointlist (&pointlist, nalloc+=512);
	pointlist.x[pointlist.npoints] = x;
	pointlist.y[pointlist.npoints] = y;
	pointlist.offset[pointlist.npoints] = offset;
	pointlist.reported[pointlist.npoints] = 0;
	pointlist.npoints++;
    }
}

int
extend_pointlist (struct pointlist *list, int n)
{
    list->x = (double *) G_realloc (list->x, n * sizeof(double));
    list->y = (double *) G_realloc (list->y, n * sizeof(double));
    list->offset = (long *) G_realloc (list->offset, n * sizeof(long));
    list->reported = (char *) G_realloc (list->reported, n * sizeof(char));
}

is_comment (buf)
    char *buf;
{
    char comment[2];

/* comments are blank lines and lines that have # as first non-blank char */
    if (sscanf (buf, "%1s", comment) != 1 || *comment == '#')
	return 1;
    return 0;
}

int
get_xy (buf, x, y, fs)
    char *buf;
    double *x, *y;
    char *fs;
{
    char ebuf[1024], nbuf[1024];

    if (!get_field (buf, 0, fs, ebuf))
	return 0;
    if (!get_field (buf, 1, fs, nbuf))
	return 0;
    if (!G_scan_northing (nbuf, y, G_projection()))
	return 0;
    if (!G_scan_easting (ebuf, x, G_projection()))
	return 0;
    return 1;
}
