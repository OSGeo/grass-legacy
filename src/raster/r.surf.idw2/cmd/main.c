#include <unistd.h>
#include "gis.h"
#include "local_proto.h"

int search_points = 12;

int npoints = 0;
int npoints_alloc = 0;
int nsearch;

struct Point
{
    double north, east;
    double z;
    double dist;
};
struct Point *points = NULL;
struct Point *list;

int 
main (int argc, char *argv[])
{
    int fd, maskfd;
    CELL *cell, *mask;
    struct Cell_head window;
    int row, col;
    double north, east;
    double dx,dy;
    double maxdist,dist;
    double sum1, sum2;
    int i,n,max;
	struct GModule *module;
    struct
    {
	struct Option *input, *npoints, *output;
    } parm;

    module = G_define_module();
    module->description =
		"Surface generation program.";
					        
    parm.input = G_define_option() ;
    parm.input->key        = "input" ;
    parm.input->type       = TYPE_STRING ;
    parm.input->required   = YES ;
    parm.input->description= "Name of input raster map" ;
    parm.input->gisprompt  = "old,cell,raster" ;

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

/* Make sure that the current projection is not lat/long */
    if ((G_projection() == PROJECTION_LL))
    {
         char msg[256];
         sprintf(msg,"lat/long databases not supported by r.surf.idw2.\nUse r.surf.idw instead!");
         G_fatal_error (msg);
         exit(1);
    }
                            
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
    read_cell (parm.input->answer);

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

    fprintf (stderr, "Interpolating raster map <%s> ... %d rows ... ",
	parm.output->answer, window.rows);

    north = window.north - window.ns_res/2.0;
    for (row = 0; row < window.rows; row++)
    {
        fprintf (stderr, "%-10d\b\b\b\b\b\b\b\b\b\b", window.rows-row);

	if (mask)
	{
	    if(G_get_map_row(maskfd, mask, row) < 0)
		exit(1);
	}
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
		/* fill list with first nsearch points */
	    for (i = 0; i < nsearch ; i++)
	    {
		dy = points[i].north - north;
		dx = points[i].east  - east;
		list[i].dist = dy*dy + dx*dx;
		list[i].z = points[i].z;
	    }
		/* find the maximum distance */
	    maxdist = list[max=0].dist;
	    for (n = 1; n < nsearch; n++)
	    {
		if (maxdist < list[n].dist)
		    maxdist = list[max=n].dist;
	    }
		/* go thru rest of the points now */
	    for ( ; i < npoints; i++)
	    {
		dy = points[i].north - north;
		dx = points[i].east  - east;
		dist = dy*dy + dx*dx;

		if (dist < maxdist)
		{
			/* replace the largest dist */
		    list[max].z = points[i].z;
		    list[max].dist = dist;
		    maxdist = list[max=0].dist;
		    for (n = 1; n < nsearch; n++)
		    {
			if (maxdist < list[n].dist)
			    maxdist = list[max=n].dist;
		    }
		}
	    }

		/* interpolate */
	    sum1 = 0.0;
	    sum2 = 0.0;
	    for (n = 0; n < nsearch; n++)
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
	    cell[col] = (CELL) (sum1/sum2 + 0.5);
	}
	G_put_map_row (fd, cell);
    }
    G_free(points);
    G_free(cell);
    G_close_cell(fd);
    fprintf (stderr, "done          \n");
    exit(0);
}

int 
newpoint (double z, double east, double north)
{
    if (npoints_alloc <= npoints)
    {
	npoints_alloc += 1024;
	points = (struct Point *) G_realloc (points,
		    npoints_alloc * sizeof (struct Point));
    }
    points[npoints].north = north;
    points[npoints].east  = east;
    points[npoints].z     = z;
    npoints++;

    return 0;
}
