/************************************************************************/
/*If necessary, please contact the author at hofierka@geomodel.sk.

(c) Copyright Jaroslav Hofierka, GeoModel, s.r.o. Bratislava 1999, 2000

This program is based on s.surf.idw grass command.
--------------------------------------------------------------------------

s.vol.idw - inverse distance - weighted interpolation program 
for generation of 3d raster (voxel) from GRASS site file
in x,y,z,w format. Output is in G3D data format.

 v. 2.0.0 - 3/2000 -  
			input: x|y|z|%w 
			output: G3D file
*/
/*	  change site format due to datetime.lib specs
*/
/*************************************************************************/ 
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <gis.h>
#include "G3d.h"

FILE *finput, *foutput;

char *input;
char *output = NULL;

void *map= NULL;

int search_points = 12;

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

int main(argc, argv) 
    int   argc;
    char *argv[];
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
    int i,n,max,sz,cnt;
    int field,  scan_int;
    
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
    parm.output->description= "Name of output 3D - G3D file" ;
    parm.output->gisprompt  = "any,grid3,3d raster";

    parm.npoints = G_define_option() ;
    parm.npoints->key        = "npoints" ;
    parm.npoints->key_desc   = "count" ;
    parm.npoints->type       = TYPE_INTEGER ;
    parm.npoints->required   = NO ;
    parm.npoints->description="Number of interpolation points";
    parm.npoints->answer = "12";

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

    if(sscanf(parm.npoints->answer,"%d", &search_points) != 1 || search_points<1)
    {
	fprintf (stderr, "%s=%s - illegal number of interpolation points\n", 
		parm.npoints->key, parm.npoints->answer);
	G_usage();
	exit(1);
    }

    scan_int=sscanf(parm.field->answer,"%d",&field);
    if ((scan_int <= 0) || field < 1)
    {
	fprintf (stderr, "%s=%s - illegal field number\n", 
		parm.field->key, parm.field->answer);
	G_usage();
	exit(1);
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
/* interpolation begins... */

    fprintf (stderr, "Note: Percentage output is status per level!\n");
    fprintf (stderr, "Interpolating raster map <%s> ... %d levels ... ",
	parm.output->answer, Nl);

	cnt=0;

	z = current_region.top + lev_res/2.0;

  for (lev = 0; lev < Nl; lev++)
  {
	
	z -=lev_res;/* daj input*/

    north = current_region.north + current_region.ns_res/2.0; 

    for (row = 0; row < Nr; row++)
    {
        /*fprintf (stderr, "%-10d\b\b\b\b\b\b\b\b\b\b", Nl-lev);*/
        G_percent (row, Nr, 2);
	if (mask)
	{
	    if(G_get_map_row(maskfd, mask, row) < 0)
		exit(1);
	}
	north -= current_region.ns_res;
	east = current_region.west - current_region.ew_res/2.0;

	for (col = 0; col < Nc; col++)
	{
	    east += current_region.ew_res;
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
		dz = points[i].z - z;
		list[i].dist = dy*dy + dx*dx + dz*dz;
		list[i].w = points[i].w;
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
		dz = points[i].z - z;
		dist = dy*dy + dx*dx + dz*dz;

		if (dist < maxdist)
		{
			/* replace the largest dist */
		    list[max].w = points[i].w;
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
		    sum1 += list[n].w / dist;
		    sum2 += 1.0/dist;
		}
		else
		{
		    sum1 = list[n].w;
		    sum2 = 1.0;
		    break;
		}
	    }
	 /*   cell[col] = (CELL) (sum1/sum2 + .5);*/

		data[cnt] = (sum1/sum2);
                value = data[cnt];
/*printf("\n %f", value);*/
		cnt++;

        G3d_putFloat (map, col, row, lev, value);

	}/* cols */
    }/*rows*/
 } /* levs*/

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
