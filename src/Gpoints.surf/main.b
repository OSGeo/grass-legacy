#include "gis.h"

int search_points = 12;

int npoints = 0;
int npoints_alloc = 0;
int nlist;
int nsearch;

struct Point
{
    double north, east;
    double z;
    double dist;
};
struct Point *points = NULL;
struct Point *list;

#define INPUT 1
#define OUTPUT 2
#define NPOINTS 3

char input[80], output[80];

struct Command_keys keys[]=
{
    {"input",INPUT},
    {"output",OUTPUT},
    {"points",NPOINTS},
    {NULL,0}
};

stash(n, s) char *s;
{
    switch(n)
    {
    case INPUT: strcpy (input, s); break;
    case OUTPUT: strcpy (output, s); break;
    case NPOINTS:
	if (sscanf(s,"%d", &search_points) != 1 || search_points <= 0)
	{
	    fprintf (stderr, "number of points must be positive\n");
	    return -1;
	}
	break;
    default: return -1;
    }
    return 0;
}

main(argc, argv) char *argv[];
{
    int fd, maskfd;
    CELL *cell, *mask;
    struct Cell_head window;
    int row, col;
    double north, east;
    double dx,dy;
    double d,dist,z;
    double radius;
    double sum1, sum2;
    int extra;
    int i,n,max;
    char *mapset, *me;

    G_gisinit(me = argv[0]);
    *input = *output = 0;
    n = G_parse_command (argc, argv, keys, stash);
    if (n != 0 || *input == 0 || *output == 0 || search_points <= 0)
    {
	if (n <= 0)
	    G_parse_command_usage (me, keys, USAGE_LONG);
	exit(1);
    }

    list = (struct Point *) G_calloc (search_points, sizeof (struct Point));

    mapset = G_find_cell (input,"");
    if (mapset == NULL)
    {
	fprintf (stderr, "%s: %s - cell file not found\n", me, input);
	exit(1);
    }
    if (G_legal_filename(output) < 0)
    {
	fprintf (stderr, "%s: %s - illegal name\n", me, output);
	exit(1);
    }

    cell = G_allocate_cell_buf();
    if ((maskfd = G_maskfd()) >= 0)
	mask = G_allocate_cell_buf();
    else
	mask = NULL;
    G_get_window (&window);

    fd = G_open_cell_old (input, mapset);
    if (fd < 0)
    {
	fprintf (stderr, "%s: %s - can't open cell file\n", me, input);
	exit(1);
    }

    fprintf (stderr, "Reading %s ...", input);

    north = window.north - window.ns_res/2.0;
    for (row = 0; row < window.rows; row++)
    {
	G_percent (row, window.rows, 2);
	north += window.ns_res;
	if (G_get_map_row_nomask (fd, cell, row) < 0)
	    exit(1);
	for (col = 0; col < window.cols; col++)
	    if (z = cell[col])
		newpoint(z,north,window.west+(col+.5)*window.ew_res);
    }
    G_percent (row, window.rows, 2);

    G_close_cell (fd);
    if (npoints == 0)
    {
	fprintf (stderr, "%s: %s - no non-zero data points found\n", me, input);
	exit(1);
    }
    nsearch = npoints < search_points ? npoints : search_points;

    fd = G_open_cell_new(output);
    if (fd < 0)
    {
	fprintf (stderr, "%s: %s - can't create cell file\n", me, output);
	exit(1);
    }

    fprintf (stderr, "Interpolating %s ...", output);

/* set radius initially to twice value required to
 * contain M points if points are evenly distributed   
 */
    radius = 2.0 * window.rows * window.ns_res * window.cols * window.ew_res / npoints;

    north = window.north - window.ns_res/2.0;
    for (row = 0; row < window.rows; row++)
    {
	if (mask)
	{
	    if(G_get_map_row(maskfd, mask, row) < 0)
		exit(1);
	}
	G_percent (row, window.rows, 2);
	north += window.ns_res;
	east = window.west - window.ew_res/2.0;
	for (col = 0; col < window.cols; col++)
	{
	    east += window.ew_res;
		/* don't interpolate outside of the mask */
	    if (mask && mask[col] == 0)
	    {
		cell[col] = 0;
		continue;
	    }
	    nlist = 0;
	    extra = 0;
		/* fill list with the closest points */
	    for(;;)
	    {
		for (i = 0; i < npoints; i++)
		{
		    dy = points[i].north - north;
		    dx = points[i].east  - east;
		    dist = dy*dy + dx*dx;
		    if (dist > radius) continue;
		    if (nlist < nsearch)
		    {
			list[nlist].z = points[i].z;
			list[nlist].dist = dist;
			nlist++;
		    }
		    else /* replace the largest dist */
		    {
			d = list[max=0].dist;
			for (n = 1; n < nlist; n++)
			{
			    if (d < list[n].dist)
				d = list[max=n].dist;
			}
			if (d > dist)
			{
			    list[max].z = points[i].z;
			    list[max].dist = dist;
			    extra++;
			}
		    }
		}
		if (extra >= nsearch) radius *= .8; /* too many points this time */
		if (nlist == nsearch)
		    break;
		radius *= 1.6;            /* not enough points */
	    }
	    sum1 = 0.0;
	    sum2 = 0.0;
	    for (n = 0; n < nlist; n++)
	    {
		if(dist = list[n].dist)
		{
		    sum1 += list[n].z / dist;
		    sum2 += 1.0/dist;
		}
		else
		{
		    sum1 = list[n].z;
		    sum2 = 1.0;
		    break;
		}
	    }
	    cell[col] = (CELL) (sum1/sum2);
	}
	G_put_map_row (fd, cell);
    }
    G_percent (row, window.rows, 2);
    G_close_cell(fd);
}

newpoint (z, north, east)
    double z, north, east;
{
    if (npoints_alloc <= npoints)
    {
/* make sure allocated memory is a power of 2 to make best use
 * of malloc() free list */
	if (npoints_alloc)
	    npoints_alloc *= 2;
	else
	    npoints_alloc = 16;

	points = (struct Point *) G_realloc (points,
		    npoints_alloc * sizeof (struct Point));
    }
    points[npoints].north = north;
    points[npoints].east  = east;
    points[npoints].z     = z;
    npoints++;
}
