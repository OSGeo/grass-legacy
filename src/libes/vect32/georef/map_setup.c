#include	<stdio.h>
#include	"map.h"
#include "georef.h"

int setup_map_reg (void)
{
	int	i ;
	int	 status ;
	int	n_points;


	/*  initiliaze use[],  no points valid  */
	for (i=0 ;  i<MAX_COOR ;  ++i)
	 {
		*(use+i) = 0 ;  	*(bx+i) = 0 ;  	*(by+i) = 0 ;
		*(residuals+i) = 0 ;
	 }

	n_points = 0 ;
	reg_cnt = 0 ;

while (1)
 {

	/*
	 *  close down curses so that Vask will work
	 */
	mysuspend() ;

	/*  go to Vask page to enter the coordinates  */
	if ((n_points =  ask_map_coor (n_points)) < 0)
		return(-1) ;

	/*
	 * back to CURSES environment
	 */
	myrespend() ;

	Clear_base() ;
	Clear_info() ;

	/*  go to curses page  and register points  */

	status =  register_map_coor( n_points) ;
	if (status < 0)
		return (-1) ;
	if (status == 1)
		break ;

 }		/*  while (1)   */


	Clear_base () ;
	Clear_info () ;


	 flush_keyboard() ;
	 return(0) ;

}

