#include "bmif.h"
#define START_ROW	xy[incr].row
#define STOP_ROW	xy[incr+1].row
#define START_COL	xy[incr].col
#define STOP_COL	xy[incr+1].col
#include <stdio.h>

save_area(category)
	int category ;
{
	int incr, i ;
	int return_value ;
	float first_cell, last_cell ;

	return_value = 0 ;

	for(incr=0; incr<n_elements; )
	{
		if (START_ROW != STOP_ROW)
		{
			/***  DEBUGGING
			* fprintf(stderr,"ERROR: start and end row not same\n") ;
			* for(i=0; i<n_elements; i += 2)
			* {
			* 	fprintf(stderr, "%d: %d %.3lf %d %.3lf\n",
			* 		i, xy[i].row, xy[i].col, 
			* 		xy[i+1].row, xy[i+1].col);
			* }
			***/

			return_value = -1 ;
			incr++ ;
			continue ;
		}

		/*
		first_cell = (int)(START_COL + .5) ;
		last_cell = (int)STOP_COL ;
		*/

		first_cell = (START_COL + 1.0) ;
		last_cell = STOP_COL ;
		if (last_cell >= first_cell)
		{
			write_record(START_ROW, first_cell, last_cell, category ) ;
		}
		incr+=2 ;
	}
  return(return_value) ;
}
