#include "gis.h"
#include "raster.h"
#include "display.h"
#include "D.h"
#include "local_proto.h"

int 
final_check (struct Cell_head *window, char name[128], char *mapset)
{
	int screen_x, screen_y ;
	int button ;
	int cur_screen_x, cur_screen_y ;

	R_open_driver();

	G_set_window(window);
	G_get_set_window(window);
	D_check_map_window(window);

	Dcell (name,mapset,0);

	screen_y  = get_map_top() ;
	screen_x = get_map_left() ;

	G_clear_screen() ;
	fprintf(stderr, "Choose one of the Options :\n") ;
	fprintf(stderr, "Buttons:\n") ;
	fprintf(stderr, "Left:   Zoom\n") ;
	fprintf(stderr, "Middle:   UnZoom\n") ;
	fprintf(stderr, "Right:  Quit\n") ;

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
			R_close_driver();
			what(name,window);
			exit(0);
		case 2:
			D_erase_window();
			D_clear_window();
			R_close_driver();
			cur_from_db(window, name, mapset);
			unzoom(window,name,mapset);
			exit(0);
		case 3:
			D_erase_window();
			D_clear_window();
			R_close_driver();
			R_open_driver();
			cur_from_db(window, name, mapset);
			G_set_window(window);
			G_get_set_window(window);
			D_check_map_window(window);

			Dcell (name,mapset,0);
			R_close_driver();
		}
	} while (button != 3 ) ;

	return 0;
}
