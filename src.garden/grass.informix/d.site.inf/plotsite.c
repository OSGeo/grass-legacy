#include "gis.h"

extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_get_d_east();

extern int D_move_abs();
extern int D_cont_abs();

plotsite (x,y, icon, size )
    double x, y;
    char *icon;
    int size;
{
    double N,S,E,W;
    struct Cell_head window;
    int D_X, D_Y ;
    double D_u_to_d_col() ;
    double D_u_to_d_row() ;

    D_X=D_Y=0;


	if(strcmp(icon,"diamond")==0 )  {
		G_get_set_window (&window);
		D_X = (int)D_u_to_d_col(x) ;
       		D_Y = (int)D_u_to_d_row(y) ;
       		R_move_abs(D_X     , D_Y+size) ;
       		R_cont_abs(D_X+size, D_Y     ) ;
        	R_cont_abs(D_X     , D_Y-size) ;
        	R_cont_abs(D_X-size, D_Y     ) ;
        	R_cont_abs(D_X     , D_Y+size) ;
	}

	else {
		if (strcmp(icon,"box") == 0 )   {
                D_X = (int)D_u_to_d_col(x) ;
                D_Y = (int)D_u_to_d_row(y) ;
                R_move_abs(D_X-size, D_Y-size) ;
                R_cont_abs(D_X-size, D_Y+size) ;
                R_cont_abs(D_X+size, D_Y+size) ;
                R_cont_abs(D_X+size, D_Y-size) ;
                R_cont_abs(D_X-size, D_Y-size) ;
		}

		else {
			if ( strcmp(icon,"plus") == 0)  {
                		D_X = (int)D_u_to_d_col(x) ;
                		D_Y = (int)D_u_to_d_row(y) ;
                		R_move_abs(D_X-size, D_Y     ) ;
                		R_cont_abs(D_X+size, D_Y     ) ;
                		R_move_abs(D_X     , D_Y-size) ;
                		R_cont_abs(D_X     , D_Y+size) ;
			}

			else {
                		D_X = (int)D_u_to_d_col(x) ;
                		D_Y = (int)D_u_to_d_row(y) ;
                		R_move_abs(D_X-size, D_Y-size) ;
                		R_cont_abs(D_X+size, D_Y+size) ;
                		R_move_abs(D_X+size, D_Y-size) ;
                		R_cont_abs(D_X-size, D_Y+size) ;
			}
		}
	}
    return 0;
}
