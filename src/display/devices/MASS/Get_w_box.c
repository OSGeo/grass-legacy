#include <stdio.h>
#include "graphics.h"

static int button_selected;

Get_location_with_box(cx, cy, wx, wy, button)
	int cx, cy ;
	int *wx, *wy ;
	int *button ;
{
	extern int SCREEN_BOTTOM ;
	int x;
	int y;
	int dx;
	int dy;
	int xref;
	int yref;
	int x1,y1;
	int x2,y2;

	int _bmouse();	/* mouse button handler */

	cy = SCREEN_BOTTOM - cy ;
	*wy = SCREEN_BOTTOM - *wy ;
	x2 = *wx ;
	y2 = *wy ;
	x1 = cx ;
	y1 = cy ;

	Mouse_init();
	Mouse_on (_bmouse);

	Mouse_reference_location (&xref, &yref) ;

	Draw_mode (DRAW_FLIP);
	mgil (x1,y1, x1,y2);
	mgil (x1,y1, x2,y1);
	mgil (x2,y2, x1,y2);
	mgil (x2,y2, x2,y1);

	for(button_selected = 0; !button_selected; )
	{
		Mouse_location (&x, &y);

		dx = x-xref ;
		dy = y-yref ;

		if (dx || dy)
		{
			Mouse_reference_location (&xref, &yref) ;
			mgil (x1,y1, x1,y2);
			mgil (x1,y1, x2,y1);
			mgil (x2,y2, x1,y2);
			mgil (x2,y2, x2,y1);
			x2+=dx ;
			y2+=dy ;
			mgil (x1,y1, x1,y2);
			mgil (x1,y1, x2,y1);
			mgil (x2,y2, x1,y2);
			mgil (x2,y2, x2,y1);
		}
	}
	mgil (x1,y1, x1,y2);
	mgil (x1,y1, x2,y1);
	mgil (x2,y2, x1,y2);
	mgil (x2,y2, x2,y1);
	Mouse_off();
	Draw_mode (DRAW_NORMAL);

	*button = button_selected;
	*wx = x2;
	*wy = SCREEN_BOTTOM - y2;
}

_bmouse (button)
{
	button_selected = button ;
}
