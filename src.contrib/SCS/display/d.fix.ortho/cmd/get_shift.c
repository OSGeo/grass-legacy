#include "gis.h"
#include "raster.h"
#include "display.h"

int 
get_shift (double *E, double *N)
{
	char buf1[50], buf2[50];
	double lat, lon ;
	int screen_x, screen_y ;
	int cur_screen_x, cur_screen_y ;
	double east=0.0, north=0.0, ieast=0.0, inorth=0.0 ;
	int button ;
	int white, black ;
	int projection;
	int draw_on ;

	projection = G_projection();
	white = D_translate_color("white") ;
	black = D_translate_color("black") ;

	screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
	screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;
	draw_on = 0 ;

	fprintf(stderr, "\n\nButtons:\n") ;
	fprintf(stderr, "Left:   Point in image\n") ;
	fprintf(stderr, "Middle: Where to put it\n") ;
	fprintf(stderr, "Right:  Do shift\n\n\n") ;

	for(;;)
	{
		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
		fprintf(stderr,"x_%d y_%d\n",screen_x, screen_y);
		if (button == 1) {
			draw_on = 1;
			ieast = D_d_to_u_col((double)screen_x) ;
			inorth = D_d_to_u_row((double)screen_y) ;
			G_format_easting  (ieast,  buf1, projection);
			G_format_northing (inorth, buf2, projection);
			fprintf(stderr,"%18s %18s image\n", buf1, buf2) ;
			}
		if (button == 2) {
			draw_on = 1;
			east = D_d_to_u_col((double)screen_x) ;
			north = D_d_to_u_row((double)screen_y) ;
			G_format_easting  (east,  buf1, projection);
			G_format_northing (north, buf2, projection);
			fprintf(stderr,"%18s %18s map\n", buf1, buf2) ;
			}
		if(button == 3)
		{
			*E = east - ieast;
			*N = north - inorth;
			fprintf (stderr, "shift east %.2f north %.2f\n",*E,*N);
			return(0) ;
		}

		if (draw_on)
		{
			if(button == 1)
			{
				R_move_abs(cur_screen_x, cur_screen_y) ;
				cur_screen_x = screen_x ;
				cur_screen_y = screen_y ;
			}
			black_and_white_line(black, white, screen_x,screen_y,cur_screen_x,cur_screen_y) ;
			cur_screen_x = screen_x ;
			cur_screen_y = screen_y ;
			R_move_abs(cur_screen_x, cur_screen_y) ;
			if(button == 1)
				draw_on = 0;
		}
		else if (button == 2)
		{
			R_move_abs(screen_x, screen_y) ;
			cur_screen_x = screen_x ;
			cur_screen_y = screen_y ;
			draw_on = 1 ;
		}
	}
}

