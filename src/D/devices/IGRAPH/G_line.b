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

	int left, rite ;
	int bot, top ;


	Screen_left(&left) ;
	Screen_rite(&rite) ;
	Screen_bot(&bot) ;
	Screen_top(&top) ;

	Enable_events (CURRENT_EVENTS) ;

/*****
	stat =  Set_trackarea( 1, left, top, rite, bot) ; 
sprintf( buff, " Set_trackarea: %d", stat) ;
write_debug(buff) ;
*****/



	stat = Mouse_motion ( WNO, (long)(MS_WORKING_AREA | MS_MOTION)) ;
sprintf( buff, " Mouse_motion: %d", stat) ;
write_debug(buff) ;

/* set all buttons to mode 1 ( up, down, single-click) */
	Set_win_button_mode( WNO, 1) ;

/*  clear buttons hits in the event queue  */
	Clear_motion_data() ;
	Clear_button_hits() ;

	base_x = cx ;
	base_y = cy ;

	while (not_finished)
	{
		Wait_for_next( CURRENT_EVENTS, &cur_events) ;
		if ( cur_events & MOTION_EVENT)
		{
			Restore_band_line() ;
			Get_motion_data( &m_wno, &mouse_x, &mouse_y,
				&work_status, &flag) ;
sprintf( buff, " In motion: x: %d,  y: %d, wno: %d,  WNO: %d",
	mouse_x,  mouse_y, m_wno, WNO) ;
write_debug(buff) ;
			Show_band_line( base_x, base_y, mouse_x, mouse_y) ;
		}
		if ( cur_events & BUTTON_EVENT)
		{
			Get_button_data( &tmp, &mouse_x, &mouse_y,
				&raw_button, &tmp, &tmp) ;
			*button = ++raw_button ;
			not_finished = 0 ;
		}
	}

	Disable_events (CURRENT_EVENTS) ;

	*wx = mouse_x ;
	*wy = mouse_y ;
write_debug("Returning") ;

}


#define  LINE_PLANE_MASK  0x101
#define  MAX_B_SIZE MAX_SCREEN_WIDTH+10

extern unsigned long VSI_PLANE_MASK ;

static  char  store_buffer[MAX_B_SIZE] ;
static  char  write_buffer[MAX_B_SIZE] ;

static  int  x1, x2 ;
static  int  y1, y2 ;

static  int  toggle_band = 0 ;


Show_band_line( base_x, base_y, mouse_x, mouse_y)
	int  base_x, base_y ;
	int  mouse_x, mouse_y ;
{

	int  i ;
	char White ;

	x1 = base_x ;
	y1 = base_y ;
	x2 = mouse_x ;
	y2 = mouse_y ;

/*  save the line */
	stat = getline (WNO, VSI_PLANE_MASK, x1, y1,
		x2, y2, store_buffer) ;
sprintf( buff, "Getting line stat: %d", stat) ;
write_debug(buff) ;

	if (first_time)
	{
	/*  get the index for white color  */
		White = (char)_get_lookup_for_color( 255, 255, 255 ) ;

	/*  Load the line buffer with white  */
		for (i=0; i < MAX_B_SIZE; i++)
			write_buffer[i] = White ;
		first_time = 0 ;
	}
		

/*  draw the line */
	stat = putline (WNO, VSI_PLANE_MASK, x1, y1,
		x2, y2, write_buffer) ;
sprintf( buff, "Putting band line stat: %d", stat) ;
write_debug(buff) ;

	toggle_band = 1 ;


}


Restore_band_line()
{
	if (toggle_band == 0)
	{
		return(-1) ;
	}

	stat = putline (WNO, VSI_PLANE_MASK, x1, y1,
		x2, y2, store_buffer) ;

sprintf( buff, "Restore band line stat: %d", stat) ;
write_debug(buff) ;

	toggle_band = 0 ;

	return(0) ;

}

