
/*
*  How many quads in the current gis window (lat, lon).
*  Written by GRASS Team, Fall of 88,  -mh
*/

#include	<stdio.h>
#include	<stdlib.h>
#include	"gis.h"
#include	"quad_structs.h"


int 
calculate_quads (struct quads_description *Q, struct Cell_head *W_ll, struct command_flags *flags, int hsize, int vsize)
{

    int  rows, cols ;

/*  compute how many rows and cols will fit in the window  */
/*  let it truncate by putting it in a 'int'  */

	if (Q->north)
		rows = (W_ll->north - Q->origin_lat)  / vsize ;
	else
		rows = (Q->origin_lat - W_ll->north) / vsize ;

	if (Q->east)
		cols = (W_ll->east - Q->origin_lon)  / hsize ;
	else
		cols = (Q->origin_lon - W_ll->east) / hsize ;


/*  At this point we know the opposite point is in the window.
*  If they want to enclose the area we can just add one to rows and cols,
*  and the opposite point will now be at the next quad point outside the area.
*/
	if( flags->encompass)
	{
		rows += 2 ;
		cols += 2 ;
	}
		

	if( rows <= 0  ||  cols <= 0)
	{
		fprintf( stderr,"\n  ERROR: Current region isn't large enough to encompass a quad .\n") ;
		fprintf( stderr,"\n  rows (lat): %d,    cols (lon): %d\n", rows, cols) ;
		exit(0) ;
	}

	fprintf (stdout,"\n Total number of quads in current region: %d\n", rows*cols) ;


   /*
   *  vector rows are the actual number of rows of vectors to make up the
   *   entire grid.   ditto for cols.
   */
	Q->num_rows =  rows ;
	Q->num_cols =  cols ;
	Q->num_vect_rows =  rows + 1 ;
	Q->num_vect_cols =  cols + 1 ;

	return(0) ;
}

