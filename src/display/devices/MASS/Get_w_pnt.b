#include "graphics.h"

static int button_number ;
static int cursor_x, cursor_y ;

Get_location_with_pointer(wx, wy, button)
	int *wx, *wy ;
	int *button ;
{
	int buttoneer() ;
	extern int SCREEN_BOTTOM ;

	*wy = SCREEN_BOTTOM - *wy ;
	mgibuttonint(buttoneer) ;

	button_number = 0 ;
	mgicursmode(07) ;
	mgicursxy(FULL_SCREEN, 06, *wx, *wy) ;
	while(!button_number) ;
	*button = button_number ;
	*wx = cursor_x ;
	*wy = SCREEN_BOTTOM - cursor_y ;
	mgicursmode(06) ;

	mgibuttonint(0) ;
}

buttoneer(row,col,button)
	int row, col,button ;
{
	mgigetcursxy(FULL_SCREEN, 06, &cursor_x, &cursor_y) ;
	switch(button) 
	{
		case 00:
			break ;
		case 01:
			button_number = 3 ;
			break ;
		case 02:
			button_number = 2 ;
			break ;
		case 04:
			button_number = 1 ;
			break ;
		default:
			button_number = 4 ;
			break ;
	}
}
