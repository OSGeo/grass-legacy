
/*
*  How many quads in the current gis window (lat, lon).
*  Written by GRASS Team, Fall of 88,  -mh
*/

#include	<stdio.h>
#include	"gis.h"
#include	"quad_structs.h"


calculate_quads (Q, W_ll, flags)
	struct  quads_description  *Q ;
	struct  Cell_head  *W_ll ;
	struct  command_flags  *flags ;
{

	double EAST, NORTH;
    int  rows, cols ;

/*  compute how many rows and cols will fit in the window  */
/*  let it truncate by putting it in a 'int'  */


	if (Q->north)
		NORTH = (W_ll->north - Q->origin_lat)  ;
	else
		NORTH = (Q->origin_lat - W_ll->north) ;

	EAST = (W_ll->east - Q->origin_lon)  ;

	NORTH = NORTH/QUAD_SIZE;
	EAST = EAST/QUAD_SIZE;

	rows = NORTH;
	cols = EAST;


/*  At this point we know the opposite point is in the window.
*  If they want to enclose the area we can just add one to rows and cols,
*  and the opposite point will now be at the next quad point outside the area.
*/
	if( flags->encompass)
	{
		rows += 1 ;
		cols += 1 ;
	}

		

	if( rows <= 0  ||  cols <= 0)
	{
		fprintf( stderr,"\n  ERROR: Current window isn't large enough to encompass a quad .\n") ;
		fprintf( stderr,"\n  rows (lat): %d,    cols (lon): %d\n", rows, cols) ;
		exit(0) ;
	}

	printf("\n Total number of quads in current window: %d\n", rows*cols) ;


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

