/*
 * Using mouse device, get a new screen coordinate and button number.
 * Button numbers must be the following values which correspond to the
 * following software meanings:
 *   1 - left button
 *   2 - middle button
 *   3 - right button
 *
 * This is called directly by the application programs.
 *
 * A "rubberband" line is used.  One end is fixed at the (cx, cy) coordinate.
 * The opposite end starts out at (*wx, *wy) and then tracks the mouse.
 * Upon button depression, the current coordinate is returned in (*wx, *wy) and
 * the button pressed in returned in *button.
 *
 *  Written by the GRASS Team in the Winter of 88.
 *
 */

#include	<tools.h>
#include	"igraphics.h"


#define  CURRENT_EVENTS   BUTTON_EVENT|MOTION_EVENT

extern int WNO ;

    /* DEBUG tools */
static  int   stat ;
static  int   first_time = 1 ;
static  char  buff[100] ;


Get_location_with_line(cx, cy, wx, wy, button)
	int cx, cy ;      /* current x,y coordinate   */
	int *wx, *wy ;    /* new x,y coordinate       */
	int *button ;     /* button pressed to return */
{

	int  tmp ;
	int  raw_button ;
	int  cur_events ;
	int  not_finished ;
	int  base_x, base_y ;


	int  m_wno ;
	int  mouse_x, mouse_y ;
	int  work_status ;
	long flag ;

	Enable_events (CURRENT_EVENTS) ;

	stat = Mouse_motion ( WNO, (long)(MS_WORKING_AREA | MS_MOTION)) ;

/* set all buttons to mode 1 ( up, down, single-click) */
	Set_win_button_mode( WNO, 1) ;

/*  clear buttons hits in the event queue  */
	Clear_motion_data() ;
	Clear_button_hits() ;

	base_x = cx ;
	base_y = cy ;

        not_finished = 1;
	while (not_finished)
	{
		Wait_for_next( CURRENT_EVENTS, &cur_events) ;
		if ( cur_events & MOTION_EVENT)
		{
			Get_motion_data( &m_wno, &mouse_x, &mouse_y,
				&work_status, &flag) ;

			Hide_cursor();
			Restore_band_line() ;
			Show_band_line( base_x, base_y, mouse_x, mouse_y) ;
			Show_cursor();
		}
		if ( cur_events & BUTTON_EVENT)
		{
			Get_button_data( &tmp, &mouse_x, &mouse_y,
				&raw_button, &tmp, &tmp) ;
			*button = ++raw_button ;
			not_finished = 0 ;
			Hide_cursor();
			Restore_band_line() ;
			Show_cursor();
		}
	}

	Disable_events (CURRENT_EVENTS) ;
	Reset_events () ;

	*wx = mouse_x ;
	*wy = mouse_y ;

}
