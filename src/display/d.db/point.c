#include "gis.h"
#include "display.h"
#include "raster.h"
#include "global.h"

int draw_point (double U_X, double U_Y, int type, int size)
{
    int D_X, D_Y;
    
    D_X = (int)D_u_to_d_col(U_X);
    D_Y = (int)D_u_to_d_row(U_Y);

    switch(type) {
	case TYPE_DIAMOND: 
	    R_move_abs(D_X     , D_Y+size) ;
	    R_cont_abs(D_X+size, D_Y     ) ;
	    R_cont_abs(D_X     , D_Y-size) ;
	    R_cont_abs(D_X-size, D_Y     ) ;
	    R_cont_abs(D_X     , D_Y+size) ;
	    break;

	case TYPE_BOX: 
	    R_move_abs(D_X-size, D_Y-size) ;
	    R_cont_abs(D_X-size, D_Y+size) ;
	    R_cont_abs(D_X+size, D_Y+size) ;
	    R_cont_abs(D_X+size, D_Y-size) ;
	    R_cont_abs(D_X-size, D_Y-size) ;
	    break;

	case TYPE_PLUS: 
	    R_move_abs(D_X-size, D_Y     ) ;
	    R_cont_abs(D_X+size, D_Y     ) ;
	    R_move_abs(D_X     , D_Y-size) ;
	    R_cont_abs(D_X     , D_Y+size) ;
	    break;

	case TYPE_X: 
	    R_move_abs(D_X-size, D_Y-size) ;
	    R_cont_abs(D_X+size, D_Y+size) ;
	    R_move_abs(D_X+size, D_Y-size) ;
	    R_cont_abs(D_X-size, D_Y+size) ;
	    break;
    }
    return (1);
}

