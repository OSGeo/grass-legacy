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
 */

#include	<tools.h>

#define  LINE_PLANE_MASK  0x101
#define  MAX_B_SIZE 2*MAX_SCREEN_WIDTH

extern int WNO ;
extern int VSI_PLANE_MASK ;

static  char  store_buffer[MAX_B_SIZE] ;
static  char  write_buffer[MAX_B_SIZE] ;


Get_location_with_line(cx, cy, wx, wy, button)
	int cx, cy ;      /* current x,y coordinate   */
	int *wx, *wy ;    /* new x,y coordinate       */
	int *button ;     /* button pressed to return */
{

	int  i ;
	int  tmp ;
	int  raw_button ;
	int  cur_events ;
	int  not_finished ;
	int  base_x, base_y ;

	char White ;

	int  m_wno ;
	int  mouse_x, mouse_y ;
	int  work_status ;
	long flag ;

    /* DEBUG tools */
	int   stat ;
	int   first_time ;
	char  buff[100] ;

	Enable_events (BUTTON_EVENT | MOTION_EVENT) ;

	stat = Mouse_motion ( WNO, (long)(MS_WORKING_AREA | MS_MOTION)) ;
sprintf( buff, " Mouse_motion: %d", stat) ;
write_debug(buff) ;

/* set all buttons to mode 1 ( up, down, single-click) */
	Set_win_button_mode( WNO, 1) ;

/*  clear buttons hits in the event queue  */
	Clear_motion_data() ;
	Clear_button_hits() ;

	not_finished = 1 ;
	first_time = 0 ;

write_debug("Getting motion data") ;
/*  find current position of mouse  */
	while (not_finished)
	{
	    if ( 0 != Get_motion_data( &m_wno, &mouse_x, &mouse_y,
			&work_status, &flag) )
	    {
if(first_time < 3)
{
sprintf( buff, " No motion") ;
write_debug(buff) ;
first_time++ ;
}
		continue ;
	    }

sprintf( buff, " m_wno: %d, WNO: %d", m_wno, WNO) ;
write_debug(buff) ;

	/*  not the same window  */
	    if ( m_wno != WNO)
		continue ;
		
	    not_finished = 0 ;
	}

	base_x = cx ;
	base_y = cy ;
write_debug("Getting line") ;
/*  save the line */
	getline (WNO, LINE_PLANE_MASK, base_x, base_y,
		mouse_x, mouse_y, store_buffer) ;

/*  get the index for white color  */
	White = (char)_get_lookup_for_color( 255, 255, 255 ) ;

/*  Load the line buffer with white  */
	for (i=0; i < MAX_B_SIZE; i++)
		write_buffer[i] = White ;
		

write_debug("Drawing line") ;
/*  draw the line */
	putline (WNO, LINE_PLANE_MASK, base_x, base_y,
		mouse_x, mouse_y, write_buffer) ;

	*wx = mouse_x ;
	*wy = mouse_y ;
	*button = 3 ;
write_debug("Returning") ;
	return ;

	not_finished = 1 ;

	while (not_finished)
	{
		Wait_for_next( BUTTON_EVENT, &cur_events) ;
		if ( ! (cur_events & BUTTON_EVENT))
			continue ;

		Get_button_data( &tmp, wx, wy, &raw_button, &tmp, &tmp) ;
		*button = ++raw_button ;
		break ;
	}

	Disable_events (BUTTON_EVENT|MOTION_EVENT) ;
	Hide_cursor() ;
}

