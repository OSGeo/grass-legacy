/*  @(#)make_univ.c	2.1  6/26/87  */

make_universe(U_north, U_south, U_east, U_west)
double *U_north ;
double *U_south ;
double *U_east ;
double *U_west ;
{
	char buffer[64] ;
	double x, y ;
	int screen_x, screen_y ;
	double ux1, uy1 ;
	double ux2, uy2 ;
	int button ;
	int cur_screen_x, cur_screen_y ;

	Clear_base() ;
	Write_base(5, "Buttons:") ;
	Write_base(6, "Left:   Establish a corner") ;
	Write_base(7, "Middle: Check coordinates") ;
	Write_base(8, "Right:  Accept universe") ;

	cur_screen_x = get_D_west() ;
	cur_screen_y = get_D_south() ;
	screen_x = cur_screen_x + 10 ;
	screen_y = cur_screen_y + 10 ;
	screen_to_utm(cur_screen_x, cur_screen_y, &ux1, &uy1) ;

	do
	{
		R_get_location_with_box(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
		if(button == 1)
		{
			cur_screen_x = screen_x ;
			cur_screen_y = screen_y ;
			screen_to_utm(cur_screen_x, cur_screen_y, &ux1, &uy1) ;
		}
		screen_to_utm(screen_x, screen_y, &ux2, &uy2) ;

		sprintf(buffer,"NORTH: %10.2f", uy1>uy2?uy1:uy2) ;
			Write_base(10, buffer) ;
		sprintf(buffer,"SOUTH: %10.2f", uy1<uy2?uy1:uy2) ;
			Write_base(11, buffer) ;
		sprintf(buffer,"WEST:  %10.2f", ux1<ux2?ux1:ux2) ;
			Write_base(12, buffer) ;
		sprintf(buffer,"EAST:  %10.2f", ux1>ux2?ux1:ux2) ;
			Write_base(13, buffer) ;

	} while (button != 3) ;

 /*  redefines current universe  */
	*U_west  =  ux1<ux2?ux1:ux2 ;
	*U_east  =  ux1>ux2?ux1:ux2 ;
	*U_south =  uy1<uy2?uy1:uy2 ;
	*U_north =  uy1>uy2?uy1:uy2 ;
}
