#include "gis.h"
#include "site.h"

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


int main(int argc, char *argv[])
{
    int fd, maskfd;
    CELL  *mask;
    DCELL *dcell;
    struct Cell_head window;
    struct GModule *module;
    int row, col;
    double north, east;
    double dx,dy;
    double maxdist,dist;
    double sum1, sum2;
    int i,n,max, field;
    void read_sites();
    char errmsg[200];
    struct
    {
	struct Option *input, *npoints, *output, *dfield;
    } parm;

    parm.input = G_define_option() ;
    parm.input->key        = "input" ;
    parm.input->type       = TYPE_STRING ;
    parm.input->required   = YES ;
    parm.input->description= "Name of input sites map" ;
    parm.input->gisprompt  = "old,site_lists,sites" ;

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

    parm.dfield = G_define_option ();
    parm.dfield->key = "field";
    parm.dfield->type = TYPE_INTEGER;
    parm.dfield->answer = "1";
    parm.dfield->multiple = NO;
    parm.dfield->required = NO;
    parm.dfield->description = "which decimal attribute (if multiple)";

    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =        
                    "Surface interpolation from sites data by Inverse "
                    "Distance Weighted algorithm.";
                    
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

    sscanf(parm.dfield->answer,"%d", &field);
    if (field < 1)
    {
      sprintf (errmsg, "Decimal attribute field 0 doesn't exist.");
      G_fatal_error (errmsg);
    }

    list = (struct Point *) G_calloc (search_points, sizeof (struct Point));


/* read the elevation points from the input sites file */
    read_sites (parm.input->answer, field);

    if (npoints == 0)
    {
	fprintf (stderr, "%s: no data points found\n", G_program_name());
	exit(1);
    }
    nsearch = npoints < search_points ? npoints : search_points;

/* get the window, allocate buffers, etc. */
    G_get_window (&window);

    /*cell = G_allocate_cell_buf();*/
    dcell=G_allocate_d_raster_buf();
    
    if ((maskfd = G_maskfd()) >= 0)
	mask = G_allocate_cell_buf();
    else
	mask = NULL;

  /*fd = G_open_cell_new(parm.output->answer);
    if (fd < 0)
    {
	fprintf (stderr, "%s: can't create %s\n", G_program_name(), parm.output->answer);
	exit(1);
    }
  */
    fd=G_open_raster_new(parm.output->answer,2);
    if (fd < 0)
    {
        fprintf (stderr, "%s: can't create %s\n", G_program_name(), parm.output->answer);
        exit(1);
    }
                                  
    fprintf (stderr, "Interpolating raster map <%s> ... %d rows ... ",
	parm.output->answer, window.rows);

    north = window.north + window.ns_res/2.0;
    for (row = 0; row < window.rows; row++)
    {
        fprintf (stderr, "%-10d\b\b\b\b\b\b\b\b\b\b", window.rows-row);

	if (mask)
	{
	    if(G_get_map_row(maskfd, mask, row) < 0)
		exit(1);
	}
	north -= window.ns_res;
	east = window.west - window.ew_res/2.0;
	for (col = 0; col < window.cols; col++)
	{
	    east += window.ew_res;
		/* don't interpolate outside of the mask */
	    if (mask && mask[col] == 0)
	    {
		dcell[col] = 0;
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
	/*    cell[col] = (CELL) (sum1/sum2 + .5);*/
	    dcell[col] = (DCELL) (sum1/sum2);
	}
	G_put_d_raster_row(fd,dcell);
    }
    G_close_cell(fd);
    fprintf (stderr, "done          \n");
    exit(0);
}

void newpoint ( double z,double east,double north)
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
