#include "gis.h"
#include "site.h"
#include "display.h"
#include "raster.h"
#include "options.h"
#include "local_proto.h"

int draw_points_diamond(struct Cell_head *window)
{
	double U_X, U_Y ;
	int D_X, D_Y ;
	int N=0;

	while(next_point(window, &U_X, &U_Y))
	{
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X     , D_Y+size) ;
		R_cont_abs(D_X+size, D_Y     ) ;
		R_cont_abs(D_X     , D_Y-size) ;
		R_cont_abs(D_X-size, D_Y     ) ;
		R_cont_abs(D_X     , D_Y+size) ;
		N++;
	}
	return (N);
}

int draw_points_box(struct Cell_head *window)
{
	double U_X, U_Y ;
	int D_X, D_Y ;
	double D_u_to_d_col() ;
	double D_u_to_d_row() ;
	int N=0;

	while(next_point(window, &U_X, &U_Y))
	{
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X-size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y+size) ;
		R_cont_abs(D_X+size, D_Y+size) ;
		R_cont_abs(D_X+size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y-size) ;
		N++;
	}
	return (N);
}

int draw_points_plus( struct Cell_head *window)
{
	double U_X, U_Y ;
	int D_X, D_Y ;
	int N=0;

	while(next_point(window, &U_X, &U_Y))
	{
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X-size, D_Y     ) ;
		R_cont_abs(D_X+size, D_Y     ) ;
		R_move_abs(D_X     , D_Y-size) ;
		R_cont_abs(D_X     , D_Y+size) ;
		N++;
	}
	return (N);
}

int draw_points_x( struct Cell_head *window)
{
	double U_X, U_Y ;
	int D_X, D_Y ;
	int N=0;

	while(next_point(window, &U_X, &U_Y))
	{
		D_X = (int)D_u_to_d_col(U_X) ;
		D_Y = (int)D_u_to_d_row(U_Y) ;
		R_move_abs(D_X-size, D_Y-size) ;
		R_cont_abs(D_X+size, D_Y+size) ;
		R_move_abs(D_X+size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y+size) ;
		N++;
	}
	return (N);
}

int next_point (
	struct Cell_head *window,
	double *U_X,
	double *U_Y)
{
        static Site *s;
	static int first=1;
	static int ndim, ndec;
	static RASTER_MAP_TYPE rtype;

	if(first){
	int nstr;
	    first=0;
	    rtype = -1;
	    G_site_describe (infile, &ndim, &rtype, &nstr, &ndec);
	    s=G_site_new_struct(rtype,ndim,nstr,ndec); 
	}

	do
        {
                if (G_site_get (infile, s) != 0)
                        return 0;
        }
        while(!G_site_in_region(s,window));

        *U_X=s->east;
        *U_Y=s->north;

	return 1;
}
