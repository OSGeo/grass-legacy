/* %W%  %G%  */
#include "bmif.h"
#define START_ROW	xy[incr1].row
#define STOP_ROW	xy[incr1+1].row
#define START_COL	xy[incr1].col
#define STOP_COL	xy[incr1+1].col
#include <stdio.h>

static int rec_num = 0 ;

save_area(xy, num_points, category)
	struct element xy[] ;
	int num_points ;
	int category ;
{
	int incr1, incr2 ;
	int first_cell, last_cell ;

	for(incr1=0; incr1<num_points; )
	{
		if (START_ROW != STOP_ROW)
		{
			fprintf(stderr,"ERROR: start and end row not same\n") ;
			for(incr2=0; incr2<num_points; incr2 += 2)
			{
				fprintf(stderr, "%d: %d %f %d %f\n",
					incr2, xy[incr2].row, xy[incr2].col, 
					xy[incr2+1].row, xy[incr2+1].col);
			}
			incr1++ ;
			continue ;
		}

		first_cell = (int)(START_COL + .5) ;
		last_cell = (int)STOP_COL ;
		if (last_cell >= first_cell)
		{
			write_record(START_ROW, first_cell, last_cell, 0, category ) ;
		}
		incr1+=2 ;
	}
}
