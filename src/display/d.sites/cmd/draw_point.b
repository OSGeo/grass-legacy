#include <stdio.h>
#include "options.h"

draw_points_diamond(size)
{
	char buffer[64] ;
	char *fgets() ;
	double U_X, U_Y ;
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;

	while(fgets(buffer, 128, infile))
	{
		sscanf(buffer,"%lf %lf", &U_X, &U_Y) ;
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X     , D_Y+size) ;
		R_cont_abs(D_X+size, D_Y     ) ;
		R_cont_abs(D_X     , D_Y-size) ;
		R_cont_abs(D_X-size, D_Y     ) ;
		R_cont_abs(D_X     , D_Y+size) ;
	}
}

draw_points_box(size)
{
	char buffer[64] ;
	char *fgets() ;
	double U_X, U_Y ;
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;

	while(fgets(buffer, 128, infile))
	{
		sscanf(buffer,"%lf %lf", &U_X, &U_Y) ;
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X-size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y+size) ;
		R_cont_abs(D_X+size, D_Y+size) ;
		R_cont_abs(D_X+size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y-size) ;
	}
}

draw_points_plus(size)
{
	char buffer[64] ;
	char *fgets() ;
	double U_X, U_Y ;
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;

	while(fgets(buffer, 128, infile))
	{
		sscanf(buffer,"%lf %lf", &U_X, &U_Y) ;
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X-size, D_Y     ) ;
		R_cont_abs(D_X+size, D_Y     ) ;
		R_move_abs(D_X     , D_Y-size) ;
		R_cont_abs(D_X     , D_Y+size) ;
	}
}

draw_points_x(size)
{
	char buffer[64] ;
	char *fgets() ;
	double U_X, U_Y ;
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;

	while(fgets(buffer, 128, infile))
	{
		sscanf(buffer,"%lf %lf", &U_X, &U_Y) ;
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X-size, D_Y-size) ;
		R_cont_abs(D_X+size, D_Y+size) ;
		R_move_abs(D_X+size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y+size) ;
	}
}
