/*  @(#)init_map.c	2.1  6/26/87  */

/**
*	There are two times init_map() will be called.  Once when the user first
*	sets up the map setup_map(),   and when the user wants to reset	the map
*	reset_map() .
*
**/

#include	<stdio.h>
#include	"map.h"


init_map ( coor_file)
	char	*coor_file ;
{
	int	i ;
	int	 status ;
	int	n_points;
	char	buff[85] ;

	FILE	*fp,	*fopen () ;

	/*  initiliaze use[],  no points valid  */
	for (i=0 ;  i<MAX_COOR ;  ++i)
	 {
		*(use+i) = 0 ;  	*(bx+i) = 0 ;  	*(by+i) = 0 ;
		*(residuals+i) = 0 ;
	 }

	Write_info(1, "DIGITIZER SETUP") ;

/*	D_setup_origin() ; */ /*ltp stuff should take care of this */

	Clear_info() ;

	n_points = 0 ;

	/*  if the coordinate file isn't there it means we ask for um  */

	if( (fp = fopen (coor_file, "r"))  != NULL)
	 {
		if ( curses_yes_no_default ( 2, " Use set of registered points from last session(y/n)? ", 1))
			n_points = load_coor_from_file (fp) ;

		fclose (fp) ;
		Write_info(2, "") ;
		Clear_info() ;
	  }

	reg_cnt = 0 ;


while (1)
 {
	/*  go to Vask page to enter the coordinates  */
	/*DEBUG*/ 
	debugf ("entering ask_map_coords\n");
	if ((n_points =  ask_map_coor (n_points)) < 0)
		return(-1) ;

	Clear_info() ;

	/*  go to curses page  and register points  */
/*DEBUG*/ 
	debugf ("entering register_map_coords\n");
	status =  register_map_coor( n_points) ;
/*DEBUG*/ 	debugf ("status = %d\n", status);
	if (status < 0)
		return (-1) ;
	if (status == 1)
		break ;

 }		/*  while (1)   */


	Clear_base () ;
	Clear_info () ;

	if ( (fp = fopen (coor_file, "w"))  != NULL)
	{
		save_coor_to_file (fp) ;
		fclose (fp) ;
	}

	 flush_keyboard() ;
	 return(0) ;

}			/*  init_map ()  */

