/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
**
**  Last modified by Dave Gerdes  1/90  for dig_head stuff
*/


#include <stdio.h>
#include <math.h>
#include "D.h"
#include "digit.h"
#include "libtrans.h"
#include "dig_curses.h"
#include "Map_proto.h"
#include "local_proto.h"

static  double  scale ;

/*  functions calculate mapscale, get map scale and check it with the user  */

int check_scale (struct Map_info *Map)
{

	int	cur_scale ;
	double	calc_scale ;
	char	buf[80] ;

	calc_scale = get_map_scale() ;  	 /*  our calculated scale  */
	cur_scale = Map->head.orig_scale ;


	while (1)
	 {
	/*  scales are within 10% of each other, accept it  */
		if ( fabs( (double)(cur_scale - calc_scale))  <  (cur_scale * .1) )
		{
			/*
			head.orig_scale = calc_scale ;
			*/
			Map->head.orig_scale = cur_scale ;
			Vect__write_head_binary (Map, &(Map->head)) ;
			return (1) ;
	
		}
		Clear_info () ;
	
		Write_info ( 1, " Map scales are not close to each other.") ;
	
		sprintf ( buf, " Current map scale: %d.   Calculated map scale: %12f", cur_scale, calc_scale) ;
		Write_info ( 2, buf) ;
	
		Write_info ( 3, " Enter new map scale or <RETURN> to use calculated scale: ") ;
	
		cur_scale = -1 ;
		Get_curses_text (buf) ;
		sscanf (buf, "%d", &cur_scale) ;

		if (cur_scale < 0)			/*  just hit return  */
		{
		    Map->head.orig_scale = calc_scale ;
		}
		/* 3.1 -dpg */
		Vect__write_head_binary (Map, &(Map->head)) ;
		return (1) ;

	 }

}


int 
calculate_map_scale (void)
{
	int	i ;
	int	screen_no ;
	char	*ptr ;

	double	x1, y1 ;
	double	x2, y2 ;
	double	X, Y ;
	double	hypot_x,  hypot_y,  hypot_avg ;
	float	d_scale ;		/*  digitizer scale  */

	transform_a_into_b ( (double)0, (double)0, &X, &Y) ;
	transform_a_into_b ( (double)10, (double)0, &x1, &y1) ;
	transform_a_into_b ( (double)0, (double)10, &x2, &y2) ;

	hypot_x = hypot ( fabs(x1-X), fabs(y1-Y)) ;
	hypot_y = hypot ( fabs(x2-X), fabs(y2-Y)) ;

	hypot_avg = (hypot_x + hypot_y) / 2.0 ;

	/*  
	*	convert digitizer units (inches) to meters by multipling by .0254 and 
	*	multiply by 10 for the number of digiter units used to compute the
	*	hypot average.	0.0254 * 10 = 0.254
	*/

	D_get_scale (&d_scale) ;		/*  get digitizer scale  */

	scale = hypot_avg / ( d_scale * (10. * dig_unit_conversion ())) ;
	/*
	scale = hypot_avg / ( d_scale * .254) ;
	*/
		
    return 0;
}


double 
get_map_scale (void)
{
	return (scale) ;
}


