/*  
*  Functions in this file:
*  register_map_coor() - registers already entered map coordinates to the
*    digitizer.
*  show_reg_menu()     - tell user which keys to press to register map 
*                           (puck or keyboard)
*  get_reg_response()  -  get user response (puck or keyboard)
*/

#include <stdio.h>
#include <unistd.h>
#include "libtrans.h"
#include "Vect.h"
#include "map.h"
#include "georef.h"

#define	LEAVE -1
#define	ADD_MORE 0
#define	ACCEPTED 1

#define BEEP	   putchar ('\007')

int 
register_map_coor (int n_points)
{
	int	i;
	int	action ;
	int	status ;
	double  X, Y ;
	char	buff[85] ;


show_reg_menu() ;

i = 0 ;

while (1)
 {

	/*  compute_transformation_coef() returns:
	*   -2,  Not enough points
	*   1,   everything is okay
	*   -1,  points weren't spread out enough
	*/

	/*  if there are enough points registered compute residuals  */
	if(reg_cnt >= MIN_COOR)
		status = compute_transformation_coef (ax, ay, bx, by, use, MAX_COOR) ;
	else
		status = -2 ;

	if(status == 1)
	{
		residuals_a_predicts_b (ax, ay, bx, by, use, MAX_COOR, residuals, &rms) ;
		show_residual_results(n_points, i ) ;
	}
	else
		show_coor_only(n_points, i, status ) ;

	action = get_reg_response( &X, &Y) ;

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
			Clear_base() ;
			Clear_info() ;
			return(ADD_MORE) ;
			break ;

		case 5:  /*  accept residuals as is  */
			switch(status)
			{
				case -2:
					Clear_info() ;
					sprintf ( buff,  "  Only %d points, need at least %d points.",
						reg_cnt ,MIN_COOR) ;
					Write_info(2, buff) ;
					sleep (3) ;
					Clear_info() ;
					if ( !leave() )
						return(LEAVE) ;
					show_reg_menu() ;
					break ;

				case 1:
					return(ACCEPTED) ;
					break ;

				default:
					Clear_info() ;
					Write_info(2, "  Points weren't spread out enough to setup the map.") ;
					sleep (3) ;
					Clear_info() ;
					show_reg_menu() ;
					break ;

	 		}	/*  switch (status)  */
			break ;

		default:  /*  nothing we know  */
			break ;

	}	/*  switch (action)  */


	/*  start back from the top  */
	if (i >= n_points)
		i = 0 ;
 }		/*  while (1)   */

/* NOT REACHED */

    return 0;
}			/*  register_map_coor()   */


int 
show_reg_menu (void)
{

	int  first_button ;
	char  buf[100] ;

	/*  for digitizers with keys to press  */
	if (D_cursor_buttons() )
	{
		/*  how are the buttons numbered on the cursor  */
		first_button = D_start_button() ;

		Write_info( 1, "            USING DIGITIZER CURSOR FOR INPUT") ;

		sprintf( buf, "  Key <%d> - register point,      Key<%d> - add more points", first_button, first_button+3) ;
		Write_info( 2, buf) ;

		sprintf( buf, "  Key <%d> - skip point,          Key<%d> - accept residuals", first_button+1, first_button+4) ;
		Write_info( 3, buf) ;

		sprintf( buf, "  Key <%d> - unregister point", first_button+2) ;
		Write_info( 4, buf) ;

		return(0) ;
	}

	/*  for digitizers with no keys to press and they have to walk across
	* the room to use the keyboard.
	*/

	Write_info( 1, "  r - register point,      A - accept residuals") ;
	Write_info( 2, "  s - skip a point") ;
	Write_info( 3, "  u - unregister point") ;
	Write_info( 4, "  a - add more points") ;

	return 0;
}

int 
get_reg_response (double *x, double *y)
{
	int  button ;
	int  xraw, yraw ;
	char  key ;


/*  we are using the cursor, keep looking until we see a button we can use */
	if (D_cursor_buttons() )
	{
		while( 1)
		{
			button =  ask_driver_raw( x, y) ;
			if (button >= 1  &&  button <= 5)
				break ;
		}

		return( button) ;
	}

/*  from this point  on we are just looking at the user with just a keyboard
*  for input,  have to return what the  user wants as an 'int' to be consistant
*  with the buttons above 
*/


	while(1)
	{
		Get_curses_char( &key) ;

		switch (key)
		{
			case 'r':
			D_clear_driver() ;
			D_read_raw( &xraw, &yraw) ;
			*x = (double)xraw ;
			*y = (double)yraw ;
				return(1) ;
				break ;
			case 's':
				return(2) ;
				break ;
			case 'u':
				return(3) ;
				break ;
			case 'a':
				return(4) ;
				break ;
			case 'A':
				return(5) ;
				break ;
			default:
				break ;
		}

	}	/*  while(1)  */

	return 0;
}
