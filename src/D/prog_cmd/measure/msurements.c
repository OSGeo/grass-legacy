/*  %W%  %G%  */

#include "gis.h"

measurements()
{
	double area, new_area ;
	double atan() ;
	double calc_area() ;
	double cell_size ;
	double cur_ux, cur_uy ;
	double first_ux, first_uy ;
	double D_d_to_u_row(), D_d_to_u_col() ;
	double hypot() ;
	double length ;
	double sq_meters ;
	double ux, uy ;
	double x, y ;
	int button ;
	int cur_screen_x, cur_screen_y ;
	int first_x, first_y ;
	int screen_x, screen_y ;
	struct Cell_head window ;
	int t, b, l, r ;

/* Set up conversion factors */
	G_get_window(&window) ;
	D_get_screen_window(&t, &b, &l, &r) ;
	D_do_conversions(&window, t, b, l, r) ;

	for(;;)
	{
		G_clear_screen() ;
		printf( "\nButtons:\n") ;
		printf( "Left:   where am i\n") ;
		printf( "Middle: set FIRST vertice\n") ;
		printf( "Right:  quit this\n") ;

		screen_y  = (t + b) / 2 ;
		screen_x  = (l + r) / 2 ;

		do
		{
			R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
			cur_uy = D_d_to_u_row((double)screen_y)  ;
			cur_ux = D_d_to_u_col((double)screen_x)  ;
			printf( "EAST: %10.2f\n", cur_ux) ;
			printf( "NORTH: %10.2f\n", cur_uy) ;
			if(button == 3)
				return(0) ;
		} while (button != 2) ;

		G_clear_screen() ;
		printf( "\nMiddle: set NEXT vertice\n") ;
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
			R_get_location_with_line(cur_screen_x,cur_screen_y,&screen_x, &screen_y, &button) ;
			uy = D_d_to_u_row((double)screen_y)  ;
			ux = D_d_to_u_col((double)screen_x)  ;
			switch (button)
			{
			case 1:
				printf( "EAST:  %10.2f\n", ux) ;
				printf( "NORTH: %10.2f\n", uy) ;
				break ;
			case 2:
				black_and_white_line(screen_x,screen_y,cur_screen_x,cur_screen_y)  ;
				new_area = calc_area(cur_ux, cur_uy, ux, uy) ;
				area += new_area ;
				length += hypot(cur_ux - ux, cur_uy - uy) ;
				printf("LEN:   %10.2f\n", length) ;
				cur_screen_x = screen_x ;
				cur_screen_y = screen_y ;
				cur_ux = ux ;
				cur_uy = uy ;
				break ;
			default:
				break ;
			}
		} while (button != 3) ;

/*
		black_and_white_line(first_x,first_y,cur_screen_x,cur_screen_y)  ;
*/

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
		printf("\nLEN:   %10.2f\n", length) ;
		printf("      %10.2f hectares\n", area / 10000 ) ;
		printf("      %10.4f sq. miles\n", area / 2589988.11 ) ;

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
