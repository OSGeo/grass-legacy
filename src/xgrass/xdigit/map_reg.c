
/*  
*  Functions in this file:
*  register_map_coor() - registers already entered map coordinates to the
*    digitizer.
*  get_reg_response()  -  get user response (puck or mouse)
*/
#include	<stdio.h>
#include	"map.h"
#include        "ginput.h" 
#include "digit.h"
#define	LEAVE -1
#define	ADD_MORE 0
#define	ACCEPTED 1

#ifdef BEEP	/* redefine BEEP for Xdigit */
#undef BEEP	
#endif

#define BEEP	  XBell (dpy, 25) 


register_map_coor ( n_points)
	int	n_points ;
{
	int	i;
	int	action ;
	int	active_point ;
	int	status ;
	double  X, Y ;
	char	buff[85] ;
	extern Widget toplevel;
	int yes_no;

debugf ("register_map n_points = %d\n", n_points);
i = 0 ;
       show_reg_ask ();
    XmUpdateDisplay (toplevel);

while (1)
 {

	/*  compute_transformation_coef() returns:
	*   -2,  Not enough points
	*   1,   everything is okay
	*   -1,  points weren't spread out enough
	*/

	/*  if there are enough points registered compute residuals  */
	if(reg_cnt >= MIN_COOR)
	{
	    status = compute_transformation_coef (ax, ay, bx, by, use, MAX_COOR) ;
	}
	else
		status = -2 ;

	if(status == 1)
	{
		residuals_a_predicts_b (ax, ay, bx, by, use, MAX_COOR, residuals, &rms) ;
		show_residual_results(n_points) ;
	}
/*	else
	{
		show_coor_only(n_points, i, status ) ;
	}
*/
/*DEBUG*/ debugf ("looking for response\n");
	action = get_reg_response( &X, &Y) ;

/*DEBUG*/ debugf ("action  = %d\n", action);
	switch (action)
	{
		case 1:  /*  point registered  */
			if (  n_points > 0 )
			{
				/*  registered before ?  */
				if (use[i] == 0)
					++reg_cnt ;
				use[i] = C_REGISTERED ;
				ax[i] = X ;
				ay[i] = Y ;
			}
			BEEP ;
			XFlush (dpy);
			XmUpdateDisplay (toplevel);
			++i ;
			break ;

		case 2:  /*  skip  point  */
			++i ;
			break ;

		case 3:  /*  unregister this point */
			if (use[i] )
			{
				use[i] = 0 ;
				--reg_cnt ;
			}
			++i ;
			break ;

		case 4:  /*  want to add more  points  */
			close_reg_ask();
			return(ADD_MORE) ;
			break ;

		case 5:  /*  accept residuals as is  */
			switch(status)
			{
				case -2:
				sprintf ( buff,  
			"Only %d points, need at least %d points. Continue?",
						reg_cnt ,MIN_COOR) ;
				if (Digtiz_Device == MOUSE)
				    yes_no = mouse_yes_no(buff);
				else
				    yes_no = ask_driver_yes_no(buff);
				if (!yes_no)
			        {
				    close_reg_ask();
				    return(LEAVE) ;
				}
				break ;

				case 1:
				    close_reg_ask();
				    return(ACCEPTED) ;
				    break ;

				default:
					make_monolog(2, "  Points weren't spread out enough to setup the map.") ;
					break ;

	 		}	/*  switch (status)  */
			break ;

		default:  /*  nothing we know  */
			break ;

	}	/*  switch (action)  */


	/*  start back from the top  */
	if (i >= n_points)
		i = 0 ;
	set_active (i);
 }		/*  while (1)   */

/* NOT REACHED */

}			/*  register_map_coor()   */
