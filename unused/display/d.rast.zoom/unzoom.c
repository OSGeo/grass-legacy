#include "gis.h"
#include "raster.h"
#include "display.h"
#include "D.h"
#include "local_proto.h"

int 
unzoom (struct Cell_head *window, char name[128], char *mapset)
{
	int screen_x, screen_y ;
	int button ;
	int cur_screen_x, cur_screen_y ;

	R_open_driver();

        fprintf (stdout,"\n Restoring the window from lastwindow\n");
	G_set_window(window);
	G_get_set_window(window);
	D_check_map_window(window);
	
	Dcell (name,mapset,0);

	screen_y  = get_map_top() ;
	screen_x = get_map_left() ;

	G_clear_screen() ;
	fprintf(stderr, "Would you like to save this window as current window:\n") ;
	fprintf(stderr, "Buttons:\n") ;
	fprintf(stderr, "Left:   Yes\n") ;
	fprintf(stderr, "Middle:   Zoom\n") ;
	fprintf(stderr, "Right:  No\n") ;

	cur_screen_x = (int)D_get_d_west() ;
	cur_screen_y = (int)D_get_d_south() ;
	screen_x = cur_screen_x + 10 ;
	screen_y = cur_screen_y + 10 ;

	do
	{
		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
		switch(button & 0x0f)
		{
		case 1:
			G_put_window(window);
			R_close_driver();
			exit(0);
		case 2:
			R_close_driver();
			what(name,window);
		case 3:
			R_close_driver();
		}
	} while (button != 3 ) ;

	return 0;
}
