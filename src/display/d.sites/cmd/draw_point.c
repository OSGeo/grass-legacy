#include "gis.h"
#include "options.h"

char *fgets();

draw_points_diamond(window)
	struct Cell_head *window;
{
	double U_X, U_Y ;
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;

	while(next_point(window, &U_X, &U_Y))
	{
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X     , D_Y+size) ;
		R_cont_abs(D_X+size, D_Y     ) ;
		R_cont_abs(D_X     , D_Y-size) ;
		R_cont_abs(D_X-size, D_Y     ) ;
		R_cont_abs(D_X     , D_Y+size) ;
	}
}

draw_points_box(window)
	struct Cell_head *window;
{
	double U_X, U_Y ;
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;

	while(next_point(window, &U_X, &U_Y))
	{
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X-size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y+size) ;
		R_cont_abs(D_X+size, D_Y+size) ;
		R_cont_abs(D_X+size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y-size) ;
	}
}

draw_points_plus(window)
	struct Cell_head *window;
{
	double U_X, U_Y ;
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;

	while(next_point(window, &U_X, &U_Y))
	{
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X-size, D_Y     ) ;
		R_cont_abs(D_X+size, D_Y     ) ;
		R_move_abs(D_X     , D_Y-size) ;
		R_cont_abs(D_X     , D_Y+size) ;
	}
}

draw_points_x(window)
	struct Cell_head *window;
{
	double U_X, U_Y ;
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;

	while(next_point(window, &U_X, &U_Y))
	{
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X-size, D_Y-size) ;
		R_cont_abs(D_X+size, D_Y+size) ;
		R_move_abs(D_X+size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y+size) ;
	}
}

next_point (window, U_X, U_Y)
	struct Cell_head *window;
	double *U_X;
	double *U_Y;
{
	char *desc;

	do
	{
		if (G_get_site (infile, U_X, U_Y, &desc) <= 0)
			return 0;
	}
	while(*U_X < window->west || *U_X > window->east ||
	      *U_Y < window->south || *U_Y > window->north) ;

	return 1;
}
