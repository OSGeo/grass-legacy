#include "graphics.h"

static int button_selected;

Get_location_with_pointer(wx, wy, button)
	int *wx, *wy ;
	int *button ;
{

	int new_x;
	int new_y;
	int dx;
	int dy;
	int xref;
	int yref;
	int x, y;
	extern int SCREEN_LEFT	 ;
	extern int SCREEN_RIGHT  ;
	extern int SCREEN_BOTTOM ;
	extern int SCREEN_TOP    ;

	int _lmouse();	/* mouse button handler */

	*wy = SCREEN_BOTTOM - *wy ;
	x = *wx ;
	y = *wy ;

	Mouse_init();
	Mouse_on (_lmouse);

	Mouse_reference_location (&xref, &yref) ;

	Draw_mode (DRAW_FLIP);
	mgil (x, SCREEN_TOP, x, SCREEN_BOTTOM);
	mgil (SCREEN_LEFT, y, SCREEN_RIGHT, y);

	for(button_selected = 0; !button_selected; )
	{
		Mouse_location (&new_x, &new_y);

		dx = new_x-xref ;
		dy = new_y-yref ;

		if (dx || dy)
		{
			Mouse_reference_location (&xref, &yref) ;
			/* "Erase" current lines */
			mgil (x, SCREEN_TOP, x, SCREEN_BOTTOM);
			mgil (SCREEN_LEFT, y, SCREEN_RIGHT, y);

			/* Figure new cursor location */
			x+=dx ;
			if(x<SCREEN_LEFT)    x=SCREEN_LEFT ;
			if(x>SCREEN_RIGHT)   x=SCREEN_RIGHT ;
			y+=dy ;
			if(y>SCREEN_BOTTOM)  y=SCREEN_BOTTOM ;
			if(y<SCREEN_TOP)     y=SCREEN_TOP ;

			/* "Draw" new lines */
			mgil (x, SCREEN_TOP, x, SCREEN_BOTTOM);
			mgil (SCREEN_LEFT, y, SCREEN_RIGHT, y);
		}
	}
	mgil (x, SCREEN_TOP, x, SCREEN_BOTTOM);
	mgil (SCREEN_LEFT, y, SCREEN_RIGHT, y);
	Mouse_off();
	Draw_mode (DRAW_NORMAL);

	*button = button_selected;
	*wx = x;
	*wy = SCREEN_BOTTOM - y;
}

static
_lmouse (button)
{
	button_selected = button ;
}
