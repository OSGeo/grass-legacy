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

main(argc, argv) 
int   argc;
char *argv[];
{
    int fd, maskfd;
    CELL *cell, *mask;
    struct Cell_head window;
    int row, col;
    double north, east;
    double dx,dy;
    double d,dist;
    double sum1, sum2;
    int i,n,max;
#ifdef USE_RADIUS
    double radius;
    int extra;
#endif
    struct
    {
	struct Option *input, *npoints, *output;
    } parm;

    parm.input = G_define_option() ;
    parm.input->key        = "input" ;
    parm.input->type       = TYPE_STRING ;
    parm.input->required   = YES ;
    parm.input->description= "Name of input sites map" ;
    parm.input->gisprompt  = "old,sites_list,sites" ;

    parm.output = G_define_option() ;
    parm.output->key        = "output" ;
    parm.output->type       = TYPE_STRING ;
    parm.output->required   = YES;
    parm.output->description= "Name of output raster map" ;
    parm.output->gisprompt  = "any,cell,raster" ;

    parm.npoints = G_define_option() ;
    parm.npoints->key        = "npoints" ;
    parm.npoints->key_desc   = "count" ;
    parm.npoints->type       = TYPE_INTEGER ;
    parm.npoints->required   = NO ;
    parm.npoints->description="Number of interpolation points";
    parm.npoints->answer = "12";


    G_gisinit(argv[0]);

    if (G_parser(argc, argv))
        exit(1);

    if (G_legal_filename(parm.output->answer) < 0)
    {
	fprintf (stderr, "%s=%s - illegal name\n", parm.output->key, parm.output->answer);
	exit(1);
    }


    if(sscanf(parm.npoints->answer,"%d", &search_points) != 1 || search_points<1)
    {
	fprintf (stderr, "%s=%s - illegal number of interpolation points\n", 
		parm.npoints->key, parm.npoints->answer);
	G_usage();
	exit(1);
    }

    list = (struct Point *) G_calloc (search_points, sizeof (struct Point));

/* read the elevation points from the input sites file */
    read_sites (parm.input->answer);

    if (npoints == 0)
    {
	fprintf (stderr, "%s: no data points found\n", G_program_name());
	exit(1);
    }
    nsearch = npoints < search_points ? npoints : search_points;

/* get the window, allocate buffers, etc. */
    G_get_set_window (&window);

    cell = G_allocate_cell_buf();

    if ((maskfd = G_maskfd()) >= 0)
	mask = G_allocate_cell_buf();
    else
	mask = NULL;

    fd = G_open_cell_new(parm.output->answer);
    if (fd < 0)
    {
	fprintf (stderr, "%s: can't create %s\n", G_program_name(), parm.output->answer);
	exit(1);
    }

    fprintf (stderr, "Interpolating %s ...", parm.output->answer);

/* set radius initially to twice value required to
 * contain M points if points are evenly distributed   
 */
#ifdef USE_RADIUS
    radius = 2.0 * window.rows * window.ns_res * window.cols * window.ew_res / npoints;
#endif

    north = window.north - window.ns_res/2.0;
    for (row = 0; row < window.rows; row++)
    {
	if (mask)
	{
	    if(G_get_map_row(maskfd, mask, row) < 0)
		exit(1);
	}
	G_percent (row, window.rows, 1);
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
#ifdef USE_RADIUS
	    extra = 0;
#endif
		/* fill list with the closest points */
	    for(;;)
	    {
		for (i = 0; i < npoints; i++)
		{
		    dy = points[i].north - north;
		    dx = points[i].east  - east;
		    dist = dy*dy + dx*dx;
#ifdef USE_RADIUS
		    if (dist > radius) continue;
#endif
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
#ifdef USE_RADIUS
			    extra++;
#endif
			}
		    }
		}
#ifdef USE_RADIUS
		if (extra >= nsearch) radius *= .8; /* too many points this time */
#endif
		if (nlist == nsearch)
		    break;
#ifdef USE_RADIUS
		radius *= 1.6;            /* not enough points */
#endif
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
    G_percent (row, window.rows, 1);
    G_close_cell(fd);
}

newpoint (z, east, north)
    double z, east, north;
{
    if (npoints_alloc <= npoints)
    {
	npoints_alloc += 128;
	points = (struct Point *) G_realloc (points,
		    npoints_alloc * sizeof (struct Point));
    }
    points[npoints].north = north;
    points[npoints].east  = east;
    points[npoints].z     = z;
    npoints++;
}
