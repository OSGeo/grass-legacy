/*
** Written by: Terry Baker 3 1992
** US Army Construction Engineering Research Lab
*/

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include "gis.h"
#include "Vect.h"
#include "dig_head.h"

main (argc, argv)
    int argc;
    char *argv[];
{

    CELL **get_z_array ();
    double *getlevels();
    void contour();

    struct Option *map;
    struct Option *levels;
    struct Option *vect;
    struct Option *min;
    struct Option *max;
    struct Option *step;
    struct Flag *quiet;


    struct Cell_head Wind;
    struct dig_head Head;
    char   mapname[1024];
    char   *name;
    char   *mapset;
    struct Map_info Map;
    CELL   **z_array;
    struct Range range; 
    int fd;
    FILE *Att;
    double *lev; 
    int nlevels;

    G_gisinit (argv[0]);

    map=G_define_option () ;
    map->key        = "input";
    map->type       = TYPE_STRING;
    map->required   = YES;
    map->gisprompt  = "old,cell,raster";
    map->description= "Name of an existing raster map" ;

    levels=G_define_option () ;
    levels->key        = "levels";
    levels->type       = TYPE_DOUBLE;
    levels->required   = NO;
    levels->multiple   = YES;
    levels->description= "List of contour levels" ;
    
    min=G_define_option () ;
    min->key        = "minlevel";
    min->type       = TYPE_DOUBLE;
    min->required   = NO;
    min->description= "Minimum contour level" ;

    max=G_define_option () ;
    max->key        = "maxlevel";
    max->type       = TYPE_DOUBLE;
    max->required   = NO;
    max->description= "Maximum contour level" ;

    step=G_define_option () ;
    step->key        = "step";
    step->type       = TYPE_DOUBLE;
    step->required   = NO;
    step->description= "Increment between contour levels" ;

    vect=G_define_option () ;
    vect->key        = "output";
    vect->type       = TYPE_STRING;
    vect->required   = YES;
    vect->description= "Name of output vector file" ;

    quiet = G_define_flag() ;
    quiet->key        = 'q';
    quiet->description = "Suppress progress report & min/max information" ;

    if (G_parser(argc, argv))
    exit (-1);

    name = map->answer;
    mapset = G_find_cell2 (name, "");
    if  (mapset == NULL)
    {
    	char msg[100];

    	sprintf  (msg, "<%s> raster file not found", name);
    	G_fatal_error  (msg);
    }

    fd = G_open_cell_old  (name, mapset);
    if  (fd < 0)
    {
    	char msg[100];

    	sprintf  (msg, "<%s> unable to open raster file\n", name);
    	G_fatal_error  (msg);
    }

 	if (G_read_range (name, mapset, &range) < 0)
		G_fatal_error ("could not read range file");

	/* get window info */
    G_get_window  (&Wind);


    if (0 > Vect_open_new (&Map, vect->answer))
        G_fatal_error ("Could not create vector file");
    
	Head.organization[0] = 0;
	Head.date[0] = 0;
    Head.your_name[0] = 0;
	sprintf(mapname,"from raster map %s",name);
    strncpy (Head.map_name, mapname, 39);	/* uses GISLIB strncpy() */
    Head.source_date[0] = 0;
    Head.orig_scale = 0;
    Head.line_3[0] = 0;
    Head.plani_zone = Wind.zone;
    Head.digit_thresh = 0;
    Head.map_thresh = 0;

    Head.W = Wind.west;
    Head.E = Wind.east;
    Head.S = Wind.south;
    Head.N = Wind.north;

  Vect_copy_head_data (&Head, &(Map.head));


	Att = G_fopen_new ("dig_att", vect->answer);

  	z_array = get_z_array (fd,Wind.rows,Wind.cols, quiet->answer);

	lev = getlevels(levels, max, min, step, range, &nlevels, quiet->answer);
	contour(lev, nlevels,  Map, z_array, Wind, Att, quiet->answer);
	
	Vect_close (&Map);
	fclose(Att);

  	exit (0);
}

/*********************************************************************/
CELL **
get_z_array (fd, nrow, ncol, quiet)
    int fd;
    int nrow, ncol;
    int quiet;
{
   CELL **z_array;
   int i;

    z_array = (CELL **) G_malloc (nrow*sizeof(CELL *));
    if (!quiet)
    {
        fprintf (stderr, "Reading data.\n");
        fprintf (stderr, "Percent complete: ");
    }
    for(i = 0; i < nrow; i++)
    {
	z_array[i] = (CELL *) G_malloc (ncol * sizeof(CELL));
        G_get_map_row (fd,z_array[i],i);
	if (!quiet)
	    G_percent (i+1, nrow , 2);
	    
    }
   return z_array;
}
/********************************************************************/
double *
getlevels(levels, max, min, step, range, num, quiet)
    struct Option *levels;	
    struct Option *max, *min;
    struct Option *step;
    struct Range range;
    int *num;
{
    double dmax, dmin, dstep;
    int nlevels, i, k, n;
    double j;
    double zmin, zmax; /* min and max data values */
    double *lev;
    double tmp;
    extern double atof();

    zmin = (double)(range.nmin? range.nmin : range.pmin);
    zmax = (double)(range.pmax? range.pmax : range.nmax);

    if(!quiet)
        fprintf(stderr, "Range of data:    min =  %lf max = %lf\n", zmin, zmax);
    
    nlevels = 0;
    if(levels->answers)
    {
	for(n = 0; levels->answers[n] != NULL; n++)
	    nlevels++;
	lev = (double *) G_malloc ((nlevels)*sizeof(double)); 
	n = nlevels;
	k = 0;
	for(i = 0; i < n; i++)
        {
	    j = atof (levels->answers[i]);
            if ((j < zmin) || (j > zmax))
            {
                nlevels--;
	    }
	    else
	    {
		lev[k] = j;
		k++;
	    }
	}
    }
    else if(step->answer)
    {
	dstep = atof (step->answer);
	dmax = (max->answer) ? atof (max->answer) : zmax - (int)zmax%(int)dstep;
	dmin = (min->answer) ? atof (min->answer) : (int)zmin%(int)dstep?
			      zmin - (int)zmin%(int)dstep +dstep: zmin;

	while (dmin < zmin)
	{
	    dmin += dstep;
	}
	while (dmin > zmax)
	{
	    dmin -= dstep;
	}
		
	while (dmax > zmax)
	{
	    dmax -= dstep;
	}
	while (dmax < zmin)
	{
	    dmax += dstep;
	}
	if (dmin > dmax)
	{
	    tmp = dmin;
	    dmin = dmax;
	    dmax = tmp;
	}
	dmin = dmin < zmin ? zmin : dmin;
	dmax = dmax > zmax ? zmax : dmax;
	if (!quiet)
	{
	    fprintf (stderr, "Minimum level will be %f\n", dmin);
	    fprintf (stderr, "Maximum level will be %f\n", dmax);
	    if (!G_yes ("Continue?", 1))
    		exit (0);
	}

	nlevels = (dmax - dmin)/dstep + 2;
	lev=(double *) G_malloc (nlevels * sizeof(double));
	for (nlevels = 0  ; dmin < dmax; dmin+=dstep)
	{
	    lev[nlevels] = dmin ;
	    nlevels++;
	}
	lev[nlevels] = dmax;
	nlevels++;
	}
 	else
	{
	    fprintf (stderr,"ERROR: Minimum parameters not set.\n");
	    fprintf (stderr,
		"Either a step or a list of levels must be specified\n");
	   G_fatal_error("");
	}
	*num=nlevels;
	return lev;
}
