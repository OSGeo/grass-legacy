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
        while(!G_site_in_region(s,window) || !site_qualify(s));

	if(outfile)
	    G_site_put (outfile, s);

        *U_X=s->east;
        *U_Y=s->north;
/*
	do
	{
		if (G_get_site (infile, U_X, U_Y, &desc) <= 0)
			return 0;
	}
	while(*U_X < window->west || *U_X > window->east ||
	      *U_Y < window->south || *U_Y > window->north) ;
*/

	return 1;
}
