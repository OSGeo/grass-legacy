/*
 * s.to.rast3
 *
 * 1/2001: added field parameter Markus Neteler
 * s.to.r3 was developed 2000 from s.vol.idw written 
 * by Jaro Hofierka
 *
 * s.to rast3 reads a sites list and writes 3d raster (voxel) maps.
 *
 *			input: x|y|z|%w 
 *			output: G3D file
 ************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "G3d.h"

/* #define DEBUG */

FILE *finput, *foutput;

char *input;
char *output = NULL;

void *map= NULL;

int search_points = 1; /* o.k. could be programmed better: no search at all!*/

int npoints = 0;
int npoints_alloc = 0;
int nsearch;

G3D_Region current_region;

struct Point
{
    double w, z, east, north;
    double dist;
};
struct Point *points = NULL;
struct Point *list;


int main(int argc, char *argv[])
{
    int fd, maskfd;
    CELL *cell, *mask;
    struct Cell_head window;
    int row, col, lev, n_lev, Nc, Nr, Nl;
    double north, east, w, z;
    double lev_res;
    int current_row, current_depth;
    double dx,dy,dz;
    double maxdist,dist;
    double sum1, sum2;
    float *data, value;
    char errmsg[200];
    int i,n,max,sz;
    int field;
    struct
    {
	struct Option *input, *npoints, *output, *field;
    } parm;

    parm.input = G_define_option() ;
    parm.input->key        = "input" ;
    parm.input->type       = TYPE_STRING ;
    parm.input->required   = YES ;
    parm.input->description= "Name of input 3D sites file" ;
    parm.input->gisprompt  = "old,site_lists,sites" ;

    parm.output = G_define_option() ;
    parm.output->key        = "output" ;
    parm.output->type       = TYPE_STRING ;
    parm.output->required   = YES;
    parm.output->description= "Name of G3D grid file" ;
    parm.output->gisprompt  = "any,grid3,3d raster" ;

    parm.field = G_define_option();
    parm.field ->key        = "field" ;
    parm.field ->type       = TYPE_INTEGER ;
    parm.field ->required   = NO ;
    parm.field ->description="Number of z-field attribute to use for calculation";
    parm.field ->answer = "1";

    G_gisinit(argv[0]);

    if (G_parser(argc, argv))
        exit(1);

    if (G_legal_filename(parm.output->answer) < 0)
    {
	fprintf (stderr, "%s=%s - illegal name\n", parm.output->key, parm.output->answer);
	exit(1);
    }

    G3d_getWindow (&current_region);
    G3d_readWindow(&current_region,NULL);

    output = parm.output->answer;
    sscanf(parm.field->answer,"%d",&field);

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

/* output allocations*/

        Nc = current_region.cols;
        Nr = current_region.rows;
        Nl = current_region.depths;
	lev_res = current_region.tb_res;
        sz = Nr * Nc * Nl;

   data = (float *) malloc( sz * sizeof(float) );

   if (!data) {
      fprintf(stderr, "Error: out of memory\n");
      exit(1);
   }

            map = G3d_openCellNew (output, G3D_FLOAT,  G3D_USE_CACHE_DEFAULT, &current_region);

            if (map == NULL)
            {
                fprintf (stderr, "\nOOPS can't create g3d file\n");
                return 0;
            }
            
/* conversion begins... */

    fprintf (stderr, "Writing 3D raster map <%s> ... %d levels ... ",
	parm.output->answer, Nl);
/*printf("\nnpoints: %d",npoints);*/

	for (i=0;i<npoints;i++) {
	lev = (int) (fabs((current_region.bottom - points[i].z - current_region.tb_res/2.)/ current_region.tb_res));
        row = (int) (fabs((current_region.south - points[i].north - current_region.ns_res/2.)/ current_region.ns_res));
        col = (int) (fabs((current_region.west - points[i].east - current_region.ew_res/2.)/ current_region.ew_res));
	value = points[i].w;
#ifdef DEBUG
 fprintf(stderr, "\ncol,row,lev,val: %d %d %d %f",col,row,lev,value);
#endif
	G3d_putFloat (map, col, row, lev, value);
	}

        free(data);

  if (! G3d_closeCell (map))
    fprintf(stderr, "main: error closing new g3d file");

/*  end of g3d write output */

    fprintf (stderr, "Done          \n");
    exit(0);
}

newpoint (w, z, east, north)
    double w, z, east, north;
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
    points[npoints].w     = w;	
    npoints++;
}
