

/*
*
*  Two functions in this file:
*
*  Check_window_refresh()    -  checks the GRASS graphics window to
*   to see if it needs to be refreshed.  If it does need to be refreshed
*   it grays out the area on the window that was touched.  To actually
*   refresh the window would mean the pixel values would have to  be
*   stored.  For a 27 inch screen that would be 1,664 X 1,248, which is
*   over two megabytes.
*
*  Mouse_window_activation() -  activates the graphics window when the
*   mouse in the window.  Deactivates the graphics window when the mouse
*   is outside the window.
*   When the window is activated the GRASS vlt (colors) are loaded
*   and the virtual screen will reflect the GRASS colors.
*   When the window is deactivated the virtual screen will hace the 
*   default colors, including the GRASS window.
*
*  Read the NOTES file for information and problems with these functions.
*
*
*  Written by the GRASS Team, Summer of 90, -mh.
*/

#include	<stdio.h>
#include	<tools.h>
#include  "colors.h"

extern int WNO ;
extern int VSNO ;


Check_window_refresh()
{ 
	int  last_color ;
	int  which_wno, vsno ;
	int  x, y ;
	int  x_lo, y_lo, x_hi, y_hi ;
	int  opmask;

	if ( 0 != Inq_refresh_area_data( &which_wno, &vsno, &x, &y,
		&x, &y, &x_lo, &y_lo, &x_hi, &y_hi, &opmask) )
	{
		return(0) ;
	}


	if ( 0 != Get_refresh_area_data( &which_wno, &vsno, &x, &y,
		&x, &y, &x_lo, &y_lo, &x_hi, &y_hi, &opmask) )
		return(0) ;
/*
	if ( which_wno != WNO)
		return(0) ;
*/

	last_color = Return_last_color() ;
	color (GRAY) ;
	rectf( WNO, x_lo, y_lo, x_hi, y_hi) ;
	color (last_color) ;

	return(0) ;
}

Mouse_window_activation()
{ 

    int  w_no ;
    int  i_x, i_y ;
    int  work_status ;
    int  active_stat ;

    /*
	Mouseposition( WNO, &i_x, &i_y, &work_status) ;
    */
	Mouse_win_position( &w_no, &i_x, &i_y, &work_status) ;
	Inq_active_status( &active_stat) ;

	if ( active_stat)
	{
		if ( w_no != WNO )
			Deactivate_process() ;

		return(0) ;
		
	}


	if ( w_no == WNO )
			Activate_process() ;
}

/*
*  After this line are functions that aren't being used.
*/


XXMouse_window_activation()
{ 

    int  w_no ;
    int  i_x, i_y ;
    int  work_status ;
    int  active_stat ;

    /*
	Mouseposition( WNO, &i_x, &i_y, &work_status) ;
    */
	Mouse_win_position( &w_no, &i_x, &i_y, &work_status) ;
	Inq_active_status( &active_stat) ;

	Mouseposition( WNO, &i_x, &i_y, &work_status) ;

	if( work_status)
	{                                 /*  inside window  */

		if ( w_no != WNO )
			return ;
		if (! active_stat)
			Activate_process() ;
		return ;
	}

	/*  outside window  */
	/*
	if (active_stat)
	*/
		Deactivate_process() ;
}



XMouse_window_activation()
{ 

    int  i_x, i_y ;
    int  work_status ;
    int  active_stat ;

	Mouseposition( WNO, &i_x, &i_y, &work_status) ;
	Inq_active_status( &active_stat) ;

	if( work_status)
	{                                 /*  inside window  */
		if (! active_stat)
			Activate_process() ;
	}
	else
	{                                 /*  outside window  */
		if (active_stat)
			Deactivate_process() ;
	}
}

