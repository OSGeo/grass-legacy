/* @(#)save_area.c	2.1   6/26/87 */
#include "ply_to_cll.h"
#define MIN_PROPORTION	.5
#define ONE_CELL	0
#define TWO_CELLS	1

static int rec_num = 0 ;

save_area(xy, num_points, category)
	struct element xy[] ;
	int num_points ;
	int category ;
{
	int incr ;
	int start_row, stop_row ;
	int last_col, last_row ;
	int first_cell, last_cell ;
	char got_one ;
	int start_col, stop_col ;
	double cell_proportion ;

	last_col = 0 ;
	last_row = 0 ;

	for(incr=0; incr<num_points; incr+=2)
	{
		start_row = xy[incr].row ;
		stop_row   = xy[incr+1].row ;
		start_col = (int)(xy[incr].col) ;
		stop_col   = (int)(xy[incr+1].col) ;

		if (start_row != stop_row)
		{
			fprintf(stderr,"start and end row not same, bye\n") ;
			for(incr=0; incr<num_points; incr+=2)
			{
				fprintf(stderr, "%d: %d %f %d %f\n",
					incr, xy[incr].row, xy[incr].col, 
					xy[incr+1].row, xy[incr+1].col);
			}
			exit(-1) ;
		}

#ifdef DEBUG
fprintf(stderr,"s_r %d  l_r %d  s_c %d  e_c %d  l_c %d  c_p %6.2f\n",
	start_row, last_row, start_col, stop_col, last_col, cell_proportion) ;
#endif DEBUG

		/* If starting on a new row or column, then we're starting a new cell
		 * Hence, the running cell_proportion is reset to 0   */

		if (start_row != last_row)
		{
			cell_proportion = 0.0 ;
			last_row = start_row ;
		}

		else if(start_col != last_col)
			cell_proportion = 0.0 ;

#ifdef DEBUG
fprintf(stderr,"s_r %d  l_r %d  s_c %d  e_c %d  l_c %d  c_p %6.2f\n",
	start_row, last_row, start_col, stop_col, last_col, cell_proportion) ;
#endif DEBUG

		got_one = 0 ;

		switch (stop_col-start_col)
		{
			case ONE_CELL:
				cell_proportion += xy[incr+1].col - xy[incr].col ;
				if(cell_proportion >= MIN_PROPORTION)
				{
					first_cell = start_col ;
					last_cell = start_col ;
					cell_proportion = 0.0 ;
					got_one = 1 ;
				}
				break ;
			case TWO_CELLS:
				cell_proportion += (double)start_col + 1.0 - xy[incr].col ;
				if(cell_proportion >= MIN_PROPORTION)
				{
					first_cell = start_col ;
					got_one = 1 ;
				}
				else
					first_cell = start_col + 1 ;

				cell_proportion = xy[incr+1].col - (double)stop_col ;
				if(cell_proportion >= MIN_PROPORTION)
				{
					last_cell = stop_col ;
					got_one = 1 ;
				}
				else
					last_cell = stop_col - 1 ;

				break ;

			default:
				cell_proportion += (double)start_col + 1.0 - xy[incr].col ;
				if(cell_proportion >= MIN_PROPORTION)
					first_cell = start_col ;
				else
					first_cell = start_col + 1 ;

				cell_proportion = xy[incr+1].col - (double)stop_col ;
				if(cell_proportion >= MIN_PROPORTION)
					last_cell = stop_col ;
				else
					last_cell = stop_col - 1 ;

				got_one = 1 ;

				break ;
		}

		last_col = stop_col ;

		if (got_one)
		{
				write_record(last_row, first_cell, last_cell, 0, category ) ;
		}
	}
}
