#include "gis.h"


measurements(color1,color2)
{
	double grid_to_meters;
	double area, new_area ;
	double atan() ;
	double calc_area() ;
	double cur_ux, cur_uy ;
	double first_ux, first_uy ;
	double D_d_to_u_row(), D_d_to_u_col() ;
	double hypot() ;
	double length ;
	double ux, uy ;
	int button ;
	int cur_screen_x, cur_screen_y ;
	int first_x, first_y ;
	int screen_x, screen_y ;
	struct Cell_head window ;
	int t, b, l, r ;

/* Set up conversion factors */
	grid_to_meters = G_database_units_to_meters_factor();
	if (grid_to_meters <= 0.0)
		grid_to_meters = 1.0;	/* KLUDGE for now */

	G_get_window(&window) ;
	D_get_screen_window(&t, &b, &l, &r) ;
	D_do_conversions(&window, t, b, l, r) ;

	for(;;)
	{
		G_clear_screen() ;
		printf( "\nButtons:\n") ;
		printf( "Left:   where am i\n") ;
		printf( "Middle: set FIRST vertex\n") ;
		printf( "Right:  quit this\n") ;

		screen_y  = (t + b) / 2 ;
		screen_x  = (l + r) / 2 ;

		do
		{
			R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
			cur_uy = D_d_to_u_row((double)screen_y)  ;
			cur_ux = D_d_to_u_col((double)screen_x)  ;
			print_en(cur_ux, cur_uy);
			if(button == 3)
				return(0) ;
		} while (button != 2) ;

		G_clear_screen() ;
		printf( "\nLeft:   where am i\n") ;
		printf( "Middle: set NEXT vertex\n") ;
		printf( "Right:  FINISH\n") ;

		R_move_abs(screen_x, screen_y) ;
		first_ux = cur_ux ;
		first_uy = cur_uy ;
		first_x = screen_x ;
		first_y = screen_y ;
		cur_screen_x = screen_x ;
		cur_screen_y = screen_y ;

		area = 0. ;
		length = 0. ;

		do
		{
			R_standard_color (color1);
			R_get_location_with_line(cur_screen_x,cur_screen_y,&screen_x, &screen_y, &button) ;
			uy = D_d_to_u_row((double)screen_y)  ;
			ux = D_d_to_u_col((double)screen_x)  ;
			switch (button)
			{
			case 1:
				print_en (ux, uy);
				break ;
			case 2:
				draw_line(screen_x,screen_y,cur_screen_x,cur_screen_y,color1,color2)  ;
				new_area = calc_area(cur_ux, cur_uy, ux, uy) ;
				area += new_area ;
				length += hypot(cur_ux - ux, cur_uy - uy) ;
				print_length(length * grid_to_meters);
				cur_screen_x = screen_x ;
				cur_screen_y = screen_y ;
				cur_ux = ux ;
				cur_uy = uy ;
				break ;
			default:
				break ;
			}
		} while (button != 3) ;

		new_area = calc_area(cur_ux, cur_uy, first_ux, first_uy) ;
		area += new_area ;

		if (area < 0.)
			area *= -1.0 ;

		G_clear_screen() ;
		printf( "\nButtons:\n") ;
		printf( "Left:   DO ANOTHER\n") ;
		printf( "Middle: \n") ;
		printf( "Right:  quit this\n") ;
/*
 * 10000 is sq meters per hectare
 * 2589988 is sq meters per sq mile
 */
		printf ("\n");
		print_length(length * grid_to_meters);
		area *= grid_to_meters * grid_to_meters;
		printf("AREA:  %10.2f hectares\n", area / 10000 ) ;
		printf("       %10.4f square miles\n", area / 2589988.11 ) ;
		printf("       %10.2f square meters\n", area) ;

		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
		if (button == 3)
			return(0) ;
	}
}

double
calc_area(x1, y1, x2, y2) 
	double x1, y1, x2, y2 ;
{
	return( (double)((x1-x2) * (y1+y2) / 2. )) ;
}
print_en(e,n)
	double e,n;
{
	char buf[100];

	G_format_easting (e, buf, G_projection());
	printf( "EAST:  %s\n", buf);
	G_format_northing (n, buf, G_projection());
	printf( "NORTH: %s\n", buf);
}

print_length(length)
	double length;
{
	printf("LEN:   %10.2f meters\n", length) ;
}
