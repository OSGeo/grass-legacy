#include "gis.h"
#include "display.h"
/****** OLD CODE
* #include "windround.h"
**********/
/*  D_do_conversions(window, t, b, l, r)
 *       struct Cell_head *window ;
 *       int t, b, l, r ;
 *
 *  Sets up conversion coefficients to translate between three 
 *  coordinate systems:
 *
 *  1.  Screen coordinates   (given by t, b, l, r values)
 *  2.  UTM coordinates      (given by values in window structure)
 *  3.  Window array coors   (given by values in window structure)
 *
 *  Once D_do_conversions is called, lots of conversion coefficients
 *  and conversion routines are available.
 *
 *  Calls to convert row and column (x and y) values in one system to
 *  another system are available.  In addition calls which return the
 *  conversion coefficients are alos provided.
 */

/* UTM coordinates.          (0,0) towards SW */
	static double U_west   ;  /*  western edge  (UTM/meters)  */
	static double U_east   ;  /*  eastern edge  (UTM/meters)  */
	static double U_south  ;  /*  southern edge (UTM/meters)  */
	static double U_north  ;  /*  northern edge (UTM/meters)  */
/* map array coordinates.    (0,0) towards NW */
	static double A_west   ;  /*  western edge  (array)       */
	static double A_east   ;  /*  eastern edge  (array)       */
	static double A_south  ;  /*  southern edge (array)       */
	static double A_north  ;  /*  northern edge (array)       */
/* dot (pixle)  coordinates. (0,0) towards NW */
	static double D_west   ;  /*  western edge  (screen dots) */
	static double D_east   ;  /*  eastern edge  (screen dots) */
	static double D_south  ;  /*  southern edge (screen dots) */
	static double D_north  ;  /*  northern edge (screen dots) */
	
/* Conversion factors */
	static double U_to_A_xconv, U_to_A_yconv ;     /* UTM to Array */
	static double A_to_D_xconv, A_to_D_yconv ;     /* Array to Dot */
	static double U_to_D_xconv, U_to_D_yconv ;     /* UTM to Dot   */

	/* others */
	static double ew_resolution ;
	static double ns_resolution ;
	static int ARRAY_ROWS ;
	static int ARRAY_COLS ;

int D_do_conversions(struct Cell_head *window , int t,int b,int l,int r)
{
	double A_hori_to_vert ;
	double D_vert, D_hori ;
	double WIND_BOT, WIND_TOP, WIND_LEFT, WIND_RITE ;
	double Dadj ;

	WIND_TOP  = (double)t ;
	WIND_BOT  = (double)b ;
	WIND_LEFT = (double)l ;
	WIND_RITE = (double)r ;
	ns_resolution = window->ns_res ;
	ew_resolution = window->ew_res ;

/* Key all coordinate limits off UTM window limits  */
	U_west  = window->west ;
	U_east  = window->east ;
	U_south = window->south ;
	U_north = window->north ;

/* Calculate Array window limits from UTM limits */
    /* OLD CODE ***********
	ARRAY_COLS = (U_east - U_west) / ew_resolution + WINDOW_ROUND ;
	ARRAY_ROWS = (U_north - U_south) / ns_resolution + WINDOW_ROUND ;
    **********************/
	ARRAY_COLS = (int)((U_east-U_west+ew_resolution/2) /ew_resolution);
	ARRAY_ROWS = (int)((U_north-U_south+ns_resolution/2) /ns_resolution);

	A_west = 0.0 ;
	A_east = (U_east - U_west) / ew_resolution ;
	A_south = (double)(ARRAY_ROWS) ;
	A_north = (double)(ARRAY_ROWS) - (U_north - U_south) / ns_resolution ;
	A_hori_to_vert = ew_resolution / ns_resolution ;

/* Calculate Dot limits from Array limits */
	D_vert = WIND_BOT - WIND_TOP ;
	D_hori = WIND_RITE - WIND_LEFT ;

	D_north = WIND_TOP ;
	D_west  = WIND_LEFT ;

	A_to_D_xconv = D_hori / ( (A_east  - A_west ) * A_hori_to_vert) ;
	A_to_D_yconv = D_vert / (A_south - A_north) ;

	if (A_to_D_xconv > A_to_D_yconv)
		A_to_D_xconv = A_to_D_yconv ;
	else
		A_to_D_yconv = A_to_D_xconv ;

	A_to_D_xconv *= A_hori_to_vert ;

	D_hori = A_to_D_xconv * (A_east  - A_west ) ;
	D_vert = A_to_D_yconv * (A_south - A_north) ;

/* Pull all edges in so picture stays centered */
	Dadj = ((WIND_BOT - WIND_TOP ) - D_vert) / 2 ;
	if (Dadj > 0.0)
	{
	    D_north = WIND_TOP + Dadj ;
	    D_south = D_north + D_vert ;
	}
	else
	{
	    D_south = WIND_BOT ;
	}
	Dadj = ((WIND_RITE - WIND_LEFT ) - D_hori) / 2 ;
	if (Dadj > 0.0)
	{
	    D_west = WIND_LEFT + Dadj ;
	    D_east = D_west + D_hori ;
	}
	else
	{
	    D_east = WIND_RITE ;
	}

	U_to_D_xconv = (D_east  - D_west ) / (U_east  - U_west ) ;
	U_to_D_yconv = (D_south - D_north) / (U_north - U_south) ;

/*
	if (t != D_north || b != D_south
	||  l != D_west  || r != D_east)
		D_reset_screen_window((int)D_north, (int)D_south, (int)D_west, (int)D_east) ;
*/

#ifdef DEBUG
	fprintf(stderr,
		" D_w %10.1f  D_e %10.1f  D_s %10.1f  D_n %10.1f\n",
		D_west, D_east, D_south, D_north) ;
	fprintf(stderr,
		" A_w %10.1f  A_e %10.1f  A_s %10.1f  A_n %10.1f\n",
		A_west, A_east, A_south, A_north) ;
	fprintf(stderr,
		" U_w %10.1f  U_e %10.1f  U_s %10.1f  U_n %10.1f\n",
		U_west, U_east, U_south, U_north) ;
	fprintf(stderr,
		" ARRAY_ROWS %d  resolution_ns %10.2f\n", ARRAY_ROWS, window->ns_res) ;
	fprintf(stderr,
		" ARRAY_COLS %d  resolution_ew %10.2f\n", ARRAY_COLS, window->ew_res) ;
	fprintf(stderr,
		" A_to_D_xconv %10.1f A_to_D_yconv %10.1f \n", 
		A_to_D_xconv, A_to_D_yconv ) ;
	fprintf(stderr,
		" BOT %10.1f  TOP %10.1f  LFT %10.1f  RHT %10.1f\n",
		WIND_BOT, WIND_TOP, WIND_LEFT, WIND_RITE) ;
		getchar() ;
#endif DEBUG

	return(0) ;
}

double D_get_u_to_a_yconv()
{ return(U_to_A_yconv) ; }

double D_get_u_to_a_xconv()
{ return(U_to_A_xconv) ; }

double D_get_a_to_d_xconv()
{ return(A_to_D_xconv) ; }

double D_get_a_to_d_yconv()
{ return(A_to_D_yconv) ; }

double D_get_u_to_d_xconv()
{ return(U_to_D_xconv) ; }

double D_get_u_to_d_yconv()
{ return(U_to_D_yconv) ; }

double D_get_u_west()
{ return(U_west) ; }

double D_get_u_east()
{ return(U_east) ; }

double D_get_u_north()
{ return(U_north) ; }

double D_get_u_south()
{ return(U_south) ; }

double D_get_a_west()
{ return(A_west) ; }

double D_get_a_east()
{ return(A_east) ; }

double D_get_a_north()
{ return(A_north) ; }

double D_get_a_south()
{ return(A_south) ; }

double D_get_d_west()
{ return(D_west) ; }

double D_get_d_east()
{ return(D_east) ; }

double D_get_d_north()
{ return(D_north) ; }

double D_get_d_south()
{ return(D_south) ; }

double D_u_to_a_row( double U_row )
{ 
	return( (double)ARRAY_ROWS - (((double)U_row - U_south)/ns_resolution) ) ;
}

double D_u_to_a_col(double U_col )
{ 
	return(((double)U_col - U_west )/ew_resolution )  ; 
}

double D_a_to_d_row(double A_row )
{ 
	return((A_row - A_north) * A_to_D_yconv + D_north);
}

double D_a_to_d_col(double A_col )
{ 
	return((A_col - A_west )*A_to_D_xconv + D_west) ; 
}

double D_u_to_d_row(double U_row )
{ 
	return((U_north - U_row) * U_to_D_yconv + D_north) ; 
}

double D_u_to_d_col( double U_col )
{ 
	return((U_col - U_west) * U_to_D_xconv + D_west) ; 
}

double D_d_to_u_row(double D_row )
{ 
	return(U_north - ((double)(D_row) - D_north)/U_to_D_yconv) ;
}

double D_d_to_u_col(double D_col )
{ 
	return(U_west + ((double)(D_col) - D_west)/U_to_D_xconv) ;
}

double D_d_to_a_row(double D_row )
{ 
	return(((double)D_row - D_north)/A_to_D_yconv + A_north) ; 
}

double D_d_to_a_col(double D_col )
{ 
	return(((double)D_col - D_west )/A_to_D_xconv + A_west) ; 
}

double D_get_ns_resolution()
{ return(ns_resolution) ; }

double D_get_ew_resolution()
{ return(ew_resolution) ; }
