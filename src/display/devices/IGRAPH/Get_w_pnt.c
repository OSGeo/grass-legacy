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
 * A pointer is used. This can be a crosshair, pointer, or cursor. 
 * It starts out at (*wx, *wy) and then tracks the mouse.
 * Upon button depression, the current coordinate is returned in (*wx, *wy) and
 * the button pressed in returned in *button.
 *
 *  Written by the GRASS Team in the Winter of 88.
 *
 */

#include	<tools.h>
#include	"igraphics.h"

extern int WNO ;

Get_location_with_pointer(wx, wy, button)
	int *wx, *wy ;    /* new x,y coordinate       */
	int *button ;     /* button pressed to return */
{

	int  tmp ;
	int  raw_button ;
	int  cur_events ;

	Enable_events (BUTTON_EVENT | MOTION_EVENT) ;

/* set all buttons to mode 1 ( up, down, single-click) */
	Set_win_button_mode( WNO, 1) ;

	/* cursor should be already on*/
	/*
	Show_cursor() ;
	*/

/*  clear buttons hits in the event queue  */
	Clear_motion_data() ;
	Clear_button_hits() ;

/*	Clear_button_data() doesn't clear out buttons hits in the queue.
	Neither does Clear_motion_data() .
	Clear_button_hits() is my own function to clear the hits.
*/

	while (1)
	{
		Wait_for_next( BUTTON_EVENT, &cur_events) ;
		if ( ! (cur_events & BUTTON_EVENT))
			continue ;

		Get_button_data( &tmp, wx, wy, &raw_button, &tmp, &tmp) ;
		*button = ++raw_button ;
		break ;
	}

	Disable_events (BUTTON_EVENT | MOTION_EVENT) ;
	Reset_events () ;

	/*
	Hide_cursor() ;
	*/
}
