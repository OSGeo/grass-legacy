

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
#include  "igraphics.h"

extern int SCREEN_LEFT;
extern int SCREEN_RIGHT;
extern int SCREEN_TOP;
extern int SCREEN_BOTTOM;

extern int WNO ;
extern int VSNO ;


#define MY_EVENTS (REFRESH_EVENT | KEYBOARD_EVENT | BUTTON_EVENT | DELETE_EVENT)

Check_window_refresh()
{
	int  last_color ;
	int vsno;
	int wno;
	int wxl = 100,
		wyl = 100,
		wxh = 600,
		wyh = 600;

	int  which_wno;

	int axl, ayl, axh, ayh;

	int cx = 250,
		cy = 250;

	int curevents;
	int tmp;

	char keybuf;
	int keycnt;

	int  txl, tyl, txh, tyh ;
	int  x_lo, y_lo, x_hi, y_hi ;
	int  opmask;


          Inq_events (&curevents);
          /*Wait_for_events (MY_EVENTS, &curevents);*/ 

		  if (curevents & REFRESH_EVENT)
		  {
/*			  fprintf (stderr, "got a refresh event"); */
			 if ( 0 != Get_refresh_area_data( &which_wno, &vsno, &txl, &tyl,
				&txh, &tyh, &x_lo, &y_lo, &x_hi, &y_hi, &opmask) )
					return(0) ;
			 else
			 {
/*				fprintf (stderr, "got something from get_refresh..\n");*/

				/*set clipbox to area needing refreshing*/

				clipbox (WNO, x_lo, y_lo, x_hi, y_hi);
				_setsize(); /*this is in Graph_Set.c*/

				/* now, return clipbox to normal size*/
				clipbox (WNO, 0, 0, 
						 (short)(SCREEN_RIGHT-SCREEN_LEFT), 
						 (short)(SCREEN_BOTTOM-SCREEN_TOP));


				return(0) ;
			 }
		  }

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
