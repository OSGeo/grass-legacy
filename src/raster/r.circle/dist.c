/* Written by Bill Brown, USA-CERL
 * January, 1993*/


#include <strings.h>
#include <math.h>
#include "gis.h"

typedef int FILEDESC;
double distance ( double *,double *);

int main(
    int argc,
    char *argv[])
{

	struct GModule *module;
    struct Option 	*coord, *out_file, *mult;
    char 		errbuf[100];
    int			*int_buf;
    struct Cell_head	w;
    FILEDESC    	cellfile = (FILEDESC) NULL;
    double east, north, pt[2], cur[2], row, col, fmult;


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
    out_file->description            = "Name for new raster file.";

    coord = G_define_option() ;
    coord->key        = "coordinate" ;
    coord->type       = TYPE_STRING ;
    coord->required   = YES ;
    coord->key_desc   = "x,y" ;
    coord->description= "The coordinate of the center (east,north)" ;

    mult = G_define_option();
    mult->key                    = "mult";
    mult->type                   = TYPE_DOUBLE;
    mult->required               = NO;
    mult->description            = "Multiplier";

    if (G_parser (argc, argv))
	exit (-1);
    
    G_scan_easting  (coord->answers[0], &east, G_projection()) ;
    G_scan_northing (coord->answers[1], &north, G_projection()) ;
    pt[0] = east;
    pt[1] = north;

    fmult=1.0;

    if(mult->answer)
	if(1 != sscanf(mult->answer,"%lf", &fmult)) fmult=1.0;

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
	    cur[1] = G_row_to_northing(row+0.5,&w);  
	    for(col=0; col < w.cols; col++){
		c = col;
		cur[0] = G_col_to_easting(col+0.5,&w);
		int_buf[c] = (int)(distance(pt, cur) * fmult);
	    }
	    G_put_map_row(cellfile, int_buf); 

	}
    }
    G_free(int_buf);
    G_close_cell(cellfile);

    
    return(1);
}



/*******************************************************************/

double distance ( double from[2],double to[2])
{
static int first=1;
   
    if(first){
	first=0;
	G_begin_distance_calculations();
    }
    return  G_distance(from[0], from[1], to[0], to[1]);
}

/**********************************************************************/
