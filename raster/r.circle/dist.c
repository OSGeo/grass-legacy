/*
 * $Id$
 * 
 * Written by Bill Brown, USA-CERL
 * January, 1993
 *
 * added min/max, -b 5/2001: Markus Neteler
 *       useful for i.(i)fft filters
 */

#include <stdlib.h>
#include <strings.h>
#include <math.h>
#include "gis.h"

typedef int FILEDESC;
double distance ( double *,double *, double, double, int);

#ifndef HUGE_VAL
#define HUGE_VAL        1.7976931348623157e+308
#endif

int main(
    int argc,
    char *argv[])
{

    struct GModule *module;
    struct Option 	*coord, *out_file, *min, *max, *mult;
    struct Flag         *flag;
    char 		errbuf[100];
    int			*int_buf;
    struct Cell_head	w;
    FILEDESC    	cellfile = (FILEDESC) NULL;
    double east, north, pt[2], cur[2], row, col, fmult;
    double fmin, fmax;
    int binary;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Creates a raster map containing concentric "
		"rings around a given point.";

    out_file = G_define_option();
    out_file->key                    = "output";
    out_file->type                   = TYPE_STRING;
    out_file->required               = YES;
    out_file->multiple               = NO;
    out_file->description            = "Name for new raster file";

    coord = G_define_option() ;
    coord->key        = "coordinate" ;
    coord->type       = TYPE_STRING ;
    coord->required   = YES ;
    coord->key_desc   = "x,y" ;
    coord->description= "The coordinate of the center (east,north)" ;

    min = G_define_option();
    min->key                    = "min";
    min->type                   = TYPE_DOUBLE;
    min->required               = NO;
    min->description            = "minimum radius for ring/circle map (in meters)";

    max = G_define_option();
    max->key                    = "max";
    max->type                   = TYPE_DOUBLE;
    max->required               = NO;
    max->description            = "maximum radius for ring/circle map (in meters)";

    mult = G_define_option();
    mult->key                    = "mult";
    mult->type                   = TYPE_DOUBLE;
    mult->required               = NO;
    mult->description            = "Multiplier";

    flag = G_define_flag();
    flag->key         = 'b' ;
    flag->description = "Generate binary raster map" ;
            
    if (G_parser (argc, argv))
	exit (-1);
    
    G_scan_easting  (coord->answers[0], &east, G_projection()) ;
    G_scan_northing (coord->answers[1], &north, G_projection()) ;
    pt[0] = east;
    pt[1] = north;

    fmult=1.0;

    if(min->answer)
	sscanf(min->answer,"%lf", &fmin);
    else
    	fmin=0;

    if(max->answer)
	sscanf(max->answer,"%lf", &fmax);
    else
    	fmax=HUGE_VAL;

    if (fmin > fmax)
	G_fatal_error("you specified min > max radius which is nonsensical");
	
    if(mult->answer)
	if(1 != sscanf(mult->answer,"%lf", &fmult)) fmult=1.0;

/* nonsense test */
    if(flag->answer && (!min->answer && !max->answer) )
    	G_fatal_error("binary flag doesn't make much sense without min and/or max radius specification.");
    	
    if(flag->answer)
	binary=1; /* generate binary pattern only, useful for MASK */
    else
        binary=0;


    G_get_set_window (&w); 

    if ((cellfile = G_open_cell_new(out_file->answer)) == -1)
    {
	sprintf(errbuf,"Unable to create cellfile for [%s]",out_file->answer);
	G_fatal_error(errbuf);
    }


    int_buf = (int *)G_malloc (w.cols * sizeof (int));
    {
	int c;

	for (row = 0; row < w.rows; row++) {
	    G_percent(row, w.rows, 2);
	    cur[1] = G_row_to_northing(row+0.5,&w);  
	    for(col=0; col < w.cols; col++){
		c = col;
		cur[0] = G_col_to_easting(col+0.5,&w);
		int_buf[c] = (int)(distance(pt, cur, fmin, fmax, binary) * fmult);
		if (int_buf[c] == 0)
		   G_set_null_value(&int_buf[c],1,CELL_TYPE);
	    }
	    G_put_raster_row(cellfile, int_buf, CELL_TYPE);

	}
    }
    G_free(int_buf);
    G_close_cell(cellfile);

    
    return(1);
}



/*******************************************************************/

double distance ( double from[2],double to[2], double min, double max, int binary)
{
    static int first=1;
    double dist;
   
    if(first){
	first=0;
	G_begin_distance_calculations();
    }
    
    dist=G_distance(from[0], from[1], to[0], to[1]);
    
    if ( (dist >= min) && (dist <= max) )
    	if (!binary)
		return dist;
	else
		return 1;
    else
    	return 0;  /* should be NULL ? */
}

/**********************************************************************/
