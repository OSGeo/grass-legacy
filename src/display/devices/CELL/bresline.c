
/*
*	this code came from Computer Language,  Nov. 86.
*	Drawing Lines and Circles.
*	Bresenham's Integer Line drawing algorithm
*
*/
#include "cell.h"

int 
bres_line (int x1, int y1, int x2, int y2)
{
	int  x,  y,  x_end,  y_end ;
	int  xinc,  yinc,  error ;
	int  delta_x,  delta_y ;

	x = x1;
	x_end = x2;
	y = y1;
	y_end = y2;

	if ((int)x == (int)x_end && (int)y == (int)y_end )
	{
		store_xy (x, y);
		return (0);
	}

	/*  generate equation  */
		delta_y = y_end - y ;
		delta_x = x_end - x ;

	/* figure out which way to move x  */
		xinc = 1 ;
		if (delta_x < 0)
		{
			delta_x = -delta_x ;
			xinc = -1 ;
		}

	/* figure out which way to move y  */
		yinc = 1 ;
		if (delta_y < 0)
		{
			delta_y = -delta_y ;
			yinc = -1 ;
		}

	if (delta_x > delta_y)
	{
		/*  always move x, decide when to move y  */
		/*  initialize the error term, and double delta x and delta y  */
		delta_y = delta_y << 1 ;
		error = delta_y  - delta_x ;
		delta_x = delta_y - (delta_x << 1) ;

		while (x != x_end)
		{
		
		    store_xy (x, y);

			if(error > 0)
			{
				y += yinc ;
				error += delta_x ;
			}
			else
				error += delta_y ;

			x += xinc ;
		}
	}
	else
	{
		/*  always move y, decide when to move x  */
		/*  initialize the error term, and double delta x and delta y  */
		delta_x = delta_x << 1 ;
		error = delta_x  - delta_y ;
		delta_y = delta_x - (delta_y << 1) ;

		while (y != y_end)
		{
		
			store_xy (x, y);

			if(error > 0)
			{
				x += xinc ;
				error += delta_y ;
			}
			else
				error += delta_x ;

			y += yinc ;
		}
	}

	/*  update the change  */
	store_xy (x, y);

	return (0);
}
