/*  @(#)p_to_area.c	2.1  6/26/87  */

#include "dlg.h"

#define		UNIV	1

point_to_area(ux, uy)
	double ux, uy ;
{
	int a ;
	int choice ;
	int gotone ;
	double cur_dist ;
	double new_dist ;

	gotone = 0 ;
	choice = 0 ;

	/*  skip universe if defined  */
	if (universe_defined)
		a = 2 ;
	else
		a = 1 ;

	for(	; a<=tot_areas; a++)
	{
		if (quick_check(a, ux, uy))
		{
			char buf[80] ;
			sprintf(buf,"%d pass quick check", a) ;
			Write_message(1, buf) ;

			if ( check_inside(a, ux, uy, &new_dist) )
			{
				sprintf(buf,"%d passes full check", a) ;
				Write_message(2, buf) ;

#ifdef DEBUG
sprintf(buf,"C: %10.2lf N: %10.2lf", cur_dist, new_dist) ;
Write_message(3, buf) ;
R_standard_color(-1) ;
plot_area(a, 0) ;
R_flush() ;
getchar() ;
R_standard_color(-10) ;
plot_area(a, 0) ;
R_flush() ;
#endif DEBUG
				if ( (++gotone == 1) || (new_dist < cur_dist) )
				{
					choice = a ;
					cur_dist = new_dist ;
				}
			}
		}
	}

	/*  not inside any area.  must be outside the map  */
	if ( ! choice ) 
		if (universe_defined)
			choice = UNIV ;

Clear_message() ;
	return(choice) ;
}
