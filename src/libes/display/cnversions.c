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
static double U_to_D_xconv, U_to_D_yconv ;     /* UTM to Dot   */
static double D_to_A_xconv, D_to_A_yconv ;     /* Dot to Array */

/* others */
static double ew_resolution ;
static double ns_resolution ;

int D_do_conversions(struct Cell_head *window , int t,int b,int l,int r)
{
	int ARRAY_ROWS, ARRAY_COLS ;
	double WIND_BOT, WIND_TOP, WIND_LEFT, WIND_RITE ;
	double D_vert, D_hori ;
	double U_vert, U_hori ;

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

	U_vert = U_north - U_south ;
	U_hori = U_east  - U_west ;

	D_hori = WIND_RITE - WIND_LEFT ;
	D_vert = WIND_BOT  - WIND_TOP ;

	U_to_D_xconv = D_hori / U_hori ;
	U_to_D_yconv = D_vert / U_vert ;

	if (U_to_D_xconv > U_to_D_yconv)
	{
		U_to_D_xconv = U_to_D_yconv ;
		D_west  = (double)(int)((WIND_LEFT + WIND_RITE - U_hori * U_to_D_xconv) / 2);
		D_east  = (double)(int)((WIND_LEFT + WIND_RITE + U_hori * U_to_D_xconv) / 2);
		D_north = WIND_TOP;
		D_south = WIND_BOT;
	}
	else
	{
		U_to_D_yconv = U_to_D_xconv ;
		D_west  = WIND_LEFT;
		D_east  = WIND_RITE;
		D_north = (double)(int)((WIND_TOP + WIND_BOT - U_vert * U_to_D_yconv) / 2);
		D_south = (double)(int)((WIND_TOP + WIND_BOT + U_vert * U_to_D_yconv) / 2);
	}

	D_hori = D_east  - D_west;
	D_vert = D_south - D_north;

	ARRAY_COLS = window->cols;
	ARRAY_ROWS = window->rows;

	A_west  = 0.0;
	A_north = 0.0;
	A_east  = (double) ARRAY_COLS;
	A_south = (double) ARRAY_ROWS;

	D_to_A_xconv = (double) ARRAY_COLS / D_hori;
	D_to_A_yconv = (double) ARRAY_ROWS / D_vert;

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
		" D_to_A_xconv %10.1f D_to_A_yconv %10.1f \n", 
		D_to_A_xconv, D_to_A_yconv ) ;
	fprintf(stderr,
		" BOT %10.1f  TOP %10.1f  LFT %10.1f  RHT %10.1f\n",
		WIND_BOT, WIND_TOP, WIND_LEFT, WIND_RITE) ;
#endif DEBUG

	return(0) ;
}

double D_get_ns_resolution(void)	{	return(ns_resolution) ;	}
double D_get_ew_resolution(void)	{	return(ew_resolution) ;	}

double D_get_u_to_d_xconv(void)		{	return(U_to_D_xconv) ;	}
double D_get_u_to_d_yconv(void)		{	return(U_to_D_yconv) ;	}

double D_get_u_west(void)		{	return(U_west) ;	}
double D_get_u_east(void)		{	return(U_east) ;	}
double D_get_u_north(void)		{	return(U_north) ;	}
double D_get_u_south(void)		{	return(U_south) ;	}

double D_get_a_west(void)		{	return(A_west) ;	}
double D_get_a_east(void)		{	return(A_east) ;	}
double D_get_a_north(void)		{	return(A_north) ;	}
double D_get_a_south(void)		{	return(A_south) ;	}

double D_get_d_west(void)		{	return(D_west) ;	}
double D_get_d_east(void)		{	return(D_east) ;	}
double D_get_d_north(void)		{	return(D_north) ;	}
double D_get_d_south(void)		{	return(D_south) ;	}

double D_u_to_a_row(double U_row)
{ 
	return (U_north - U_row) / ns_resolution;
}

double D_u_to_a_col(double U_col)
{ 
	return (U_col - U_west) / ew_resolution; 
}

double D_a_to_d_row(double A_row)
{ 
	return D_north + (A_row - A_north) / D_to_A_yconv;
}

double D_a_to_d_col(double A_col)
{ 
	return D_west + (A_col - A_west) / D_to_A_xconv; 
}

double D_u_to_d_row(double U_row)
{ 
	return D_north + (U_north - U_row) * U_to_D_yconv; 
}

double D_u_to_d_col(double U_col)
{ 
	return D_west + (U_col - U_west) * U_to_D_xconv; 
}

double D_d_to_u_row(double D_row)
{ 
	return U_north - (D_row - D_north) / U_to_D_yconv;
}

double D_d_to_u_col(double D_col)
{ 
	return U_west + (D_col - D_west) / U_to_D_xconv;
}

double D_d_to_a_row(double D_row)
{ 
	return A_north + (D_row - D_north) * D_to_A_yconv; 
}

double D_d_to_a_col(double D_col)
{ 
	return A_west + (D_col - D_west) * D_to_A_xconv;
}
