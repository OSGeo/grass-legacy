/* @(#)find_line.c	2.2   9/21/87 */
#include "bmif.h"
#include "gis.h"

/*
*	part of this code came from Computer Language,  Nov. 86.
*	Drawing Lines and Circles.
*	uses left shifts to avoid using floating point arithmetic.
*
*/

find_line(array, num_verticies )
	double array[] ;
	int num_verticies ;
{
	int node ;
	
	int  x,  y,  x_end,  y_end ;
	int  xinc,  yinc,  error ;
	int  delta_x,  delta_y ;

	char *G_realloc() ;

	n_elem_alloc  = 5000 ;
	xy = (struct element *) G_calloc(n_elem_alloc, sizeof(struct element)) ;

	/* adjust Y grid coordinates to Y array coordinates */
	line_yadjust(array, num_verticies) ;
/*
*line_xadjust(array, num_verticies) ;
*/

	n_elements = 0 ;

	num_verticies-- ;    /* Adjustment for the number of vert pairs */
  for (node=0; node<num_verticies; node++)
  {
		int i ;
		i = node * 2 ;
		x = (int)array[i] ;
		y = (int)array[i+1] ;
		x_end = (int)array[i+2] ;
		y_end = (int)array[i+3] ;
#ifdef DEBUG
	fprintf(stderr,"node %d: (x,y) %d:%d %d:%d\n",
		node, x, y, x_end, y_end) ;
#endif DEBUG

    /**  this is a line there shouldn't be any islands
		if ( (x == 0.0 && y == 0.0) ||
			 (x_end == 0.0 && y_end == 0.0) ) 
			continue ;
    **/
		if ((int)x == (int)x_end && (int)y == (int)y_end )
			continue ;

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
		
		/*  save this point  */
			if (n_elements + 1 >= n_elem_alloc)
			{
				n_elem_alloc = n_elem_alloc + 1000 ;
				xy = (struct element *)
					G_realloc((char *)xy, n_elem_alloc * sizeof(struct element));
			}

			xy[n_elements].row = y ;
			xy[n_elements].col = (double)x ;
			n_elements++ ;

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
		
		/*  save this point  */
			if (n_elements + 1 >= n_elem_alloc)
			{
				n_elem_alloc = n_elem_alloc + 1000 ;
				xy = (struct element *)
					G_realloc((char *)xy, n_elem_alloc * sizeof(struct element));
			}

			xy[n_elements].row = y ;
			xy[n_elements].col = (double)x ;
			n_elements++ ;

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
		array[i+2] = (double)x ;
		array[i+3] = (double)y ;
#ifdef DEBUG
	if ( ! y)
		fprintf(stderr," Line just printed a zero\n") ;
#endif DEBUG

  }  	/*  for (node)   */

}
