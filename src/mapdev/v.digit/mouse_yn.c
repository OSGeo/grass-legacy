/*  @(#)mouse_yn.c	1.1  6/26/87  */
#include <stdio.h>
#include "digit.h"
#include "local_proto.h"
#include "raster.h"
#include "dig_curses.h"
#include "glocale.h"

int mouse_yes_no (char *header)
{
	int button ;
	int	screen_x, screen_y ;

	screen_x = screen_y = 1;

	_Clear_base () ;
	Write_base(10, header) ;
	Write_base(12, _("    Buttons:")) ;
	Write_base(13, _("       Left:   Yes")) ;
	Write_base(14, _("       Middle: No")) ;
	Write_base(15, _("       Right:  No")) ;


	R_get_location_with_pointer ( &screen_x, &screen_y, &button) ;

	return (button == LEFTB) ;
}

/* this is for node_lines () */
int mouse_next_prev (char *header)
{
	int button ;
	int	screen_x, screen_y ;

	_Clear_base () ;
	Write_base(10, header) ;
	Write_base(12, _("    Buttons:")) ;
	Write_base(13, _("       Left:   Previous line")) ;
#ifdef ANOTHER_BUTTON
	Write_base(14, _("       Middle: Quit")) ;
	Write_base(15, _("       Right:  Next line")) ;
#else
	Write_base(14, _("       Middle: Next line")) ;
	Write_base(15, _("       Right:  Quit")) ;
#endif

	R_get_location_with_pointer ( &screen_x, &screen_y, &button) ;

	return(button) ;
}

int mouse_yes_no_zoom (char *header,
		unsigned char type, struct line_pnts *Xpoints)
{
	char buf[100];
	int button ;
	int	screen_x, screen_y ;

	screen_x = screen_y = 1;

	sprintf(buf, _("       Middle: Zoom%s"),
			(type == LINE || type == AREA ? _("/Continue") : "")) ;
	_Clear_base () ;
	Write_base(10, header) ;
	Write_base(12, _("    Buttons:")) ;
	Write_base(13, _("       Left:   Yes")) ;
	Write_base(14, buf) ;
	Write_base(15, _("       Right:  No")) ;


	R_get_location_with_pointer ( &screen_x, &screen_y, &button) ;

	if(button == 2)
		zoom_window (type, Xpoints);

	return (button) ;
}

