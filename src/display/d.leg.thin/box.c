#include "gis.h"


get_legend_box (x0, x1, y0, y1)
int *x0, *x1, *y0, *y1;
{
    int screen_x, screen_y ;
    int button ;
    int cur_screen_x, cur_screen_y ;

    D_get_screen_window(&cur_screen_y, &screen_y, &cur_screen_x, &screen_x);

    fprintf(stderr, "\n\n");
    fprintf(stderr, "Buttons:\n") ;
    fprintf(stderr, "Left:   Establish a corner\n") ;
    fprintf(stderr, "Right:  Accept box for legend\n\n") ;

    do
    {
	R_get_location_with_box(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
	button &= 0xf;


	switch(button)
	{
	case 1:
		cur_screen_x = screen_x ;
		cur_screen_y = screen_y ;
		break ;
	case 2:
	case 3: break;
	}

    } while (button != 3) ;

    *x0 = cur_screen_x;
    *x1 = screen_x;
    *y0 = cur_screen_y;
    *y1 = screen_y;

    fprintf (stderr, "\n");
    return(1);

}



