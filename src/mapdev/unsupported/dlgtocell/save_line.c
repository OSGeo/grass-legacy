/* @(#)save_line.c	2.2   9/21/87 */
#include <stdio.h>
#include "bmif.h"

save_line(category)
	int category ;
{
	int incr ;
	int cur_row ;
	int	stat ;
	float start_col ;
	float stop_col ;

	stat = 0 ;
	cur_row = xy[0].row ;
	start_col = xy[0].col ;
	stop_col  = xy[0].col ;

	for(incr=1; incr<n_elements; incr++)
	{
		if (xy[incr].row != cur_row)
		{
			if(stop_col < xy[incr].col)
			{
				stat += write_record( cur_row,
					(start_col),
					(float)( (stop_col + xy[incr].col) / 2.0 ),
					category);

				start_col = (stop_col + xy[incr].col) / 2.0 ;
				stop_col = xy[incr].col ;

			}
			else  if(start_col > xy[incr].col)
			{
				stat += write_record( cur_row,
					(float)( (start_col + xy[incr].col) / 2.0 ),
					(float)(stop_col ),
					category);

				stop_col = (start_col + xy[incr].col) / 2.0 ;
				start_col = xy[incr].col ;
			}
			else
			{
				stat += write_record(cur_row,
					(start_col),
					(stop_col),
					category);

				start_col = xy[incr].col ;
				stop_col = xy[incr].col ;
			}
			cur_row = xy[incr].row ;
		}
		else
		{
			if (start_col > xy[incr].col)
				start_col = xy[incr].col ;
			if (stop_col < xy[incr].col)
				stop_col = xy[incr].col ;
		}
	}
	stat += write_record(cur_row,
		(start_col),
		(stop_col),
		category);

	return(stat) ;

}

