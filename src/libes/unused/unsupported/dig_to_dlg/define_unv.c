/*  @(#)define_univ.c	1.2  6/24/87  */

/*  file contains  define_universe()  and  box_universe() 
*/

#include	<stdio.h>
#include	"structures.h"
#include	"dlg.h"
#include	"mode.h"
#include	"universe.h"

#define		OUTSIDE_UNIV	1
#define		INSIDE_UNIV		2
#define		LARGE_NUM		999999999.9 ;

define_universe (f_digit)
	FILE	*f_digit ;
{

	double	n_edge, s_edge, e_edge, w_edge ;
	double	n_limit, s_limit, e_limit, w_limit ;

	int	i ;
	char	answer[30] ;


/*  find smallest univ box that will encompass all the endpoints  */

	n_limit = 0 ;
	s_limit = LARGE_NUM ;
	e_limit = 0 ;
	w_limit = LARGE_NUM ;

	for ( i = 0; i < n_endpts; i++)
	 {
		if (endpoints[i].y > n_limit)
			n_limit = endpoints[i].y ;
		if (endpoints[i].y < s_limit)
			s_limit = endpoints[i].y ;
		if (endpoints[i].x > e_limit)
			e_limit = endpoints[i].x ;
		if (endpoints[i].x < w_limit)
			w_limit = endpoints[i].x ;
	 }

	if ( ! box_universe ( n_limit, s_limit, e_limit, w_limit, 1) )
	{
		Write_info ( 2, "areas digitized outside map edges, redraw window." ) ;
		sleep(2) ;
		if (do_window() != -1)
			redraw_window(f_digit) ;
	}

	plot_points (LINE, UNIV_N_POINTS, univ_xarray, univ_yarray,
			"yellow", "none" ) ;


	while (1)
	{

/*  show the menu  */
		Clear_base() ;
		Clear_info() ;
		Write_base( 3, "  r -  redraw universe (using mouse)") ;
		Write_base( 4, "  m -  use map edges as universe") ;
		Write_base( 5, "  z -  use zoom to re-window") ;
		Write_base( 6, "") ;
		Write_base( 7, "  a -  accept current universe") ;

		Write_info( 3, "      enter letter: ") ;
		Get_curses_char( answer) ;


		switch (*answer)
		{
			case 'r':
			case 'm':
					break ;

			case 'z':
					if (do_window() != -1)
					{
						redraw_window(f_digit) ;
						plot_points (LINE, UNIV_N_POINTS, univ_xarray,
							univ_yarray, "yellow", "none" ) ;
						continue ;
					}
					break ;

			case 'a':
					break ;
			default:
					Write_info( 4, "   - unknown command") ;
					sleep(1) ;
					Write_info( 4, "") ;
					continue ;
					break ;


		}

		if (*answer == 'a')
			break ;

		plot_points (LINE, UNIV_N_POINTS, univ_xarray, univ_yarray,
					"black", "none" ) ;

		if (*answer == 'r')
		 {
			Write_info( 2, " Use mouse to define universe." ) ;
			Write_info( 3, "" ) ;
			Write_info( 4, "" ) ;
			make_universe( &n_edge, &s_edge, &e_edge, &w_edge ) ;
		 }
		else
		 {
			w_edge = dlg_coors[SW].utm_e + 1 ;
	 		s_edge = dlg_coors[SW].utm_n + 1 ;
	   		n_edge = dlg_coors[NW].utm_n - 1 ;
			e_edge = dlg_coors[NE].utm_e - 1 ;
		 }

		if ( ! box_universe( n_edge, s_edge, e_edge, w_edge, 0) )
		{
			Write_info ( 4, " universe drawn outside map edges" ) ;
			sleep(2) ;
			fflush (stdout) ;
			putchar (007) ;
			fflush (stdout) ;
		}

		plot_points (LINE, UNIV_N_POINTS, univ_xarray, univ_yarray,
					"yellow", "none" ) ;

			/*  is new universe to small for map  */
		if ( n_edge <= n_limit  ||  s_edge >= s_limit
		  || e_edge <= e_limit  ||  w_edge >= w_limit )
		{
			Write_info ( 4, " universe didn't encompass all areas!!" ) ;
			sleep(2) ;
			fflush (stdout) ;
			putchar (007) ;
			fflush (stdout) ;
		}
		Clear_base() ;

	}		/*  while ( accept universe?)  */

	plot_points (AREA, UNIV_N_POINTS, univ_xarray, univ_yarray, "gray", "none" ) ;

	/*  put centroid in center of map  */
	 areas[INSIDE_UNIV].cent_x =  dlg_coors[SW].utm_e  +
			(dlg_coors[SE].utm_e -  dlg_coors[SW].utm_e) / 2  ;
	 areas[INSIDE_UNIV].cent_y =  dlg_coors[SW].utm_n  +
			(dlg_coors[NW].utm_n -  dlg_coors[SW].utm_n) / 2  ;

	areas[OUTSIDE_UNIV].cent_y = areas[INSIDE_UNIV].cent_y ;
	areas[OUTSIDE_UNIV].cent_x = areas[INSIDE_UNIV].cent_x ;

	Clear_base() ;

}	 /*  define_universe()  */


/*
*  box_universe() creates a box out of 4 edges inside the bounding area.
*  if halfit is true; it creates the box half the distance between the passed
*  box and the map edges.
*  return 1 - if box okay
*  return 0 - if box outside bounding box (window)
*/

box_universe( N, S, E, W, halfit)
	double	N,	S,	E,	W ;
	int		halfit ;
{

	double	multiplier ;

	if (halfit)
		multiplier = 0.5 ;
	else
		multiplier = 0.0 ;

	/*  box direction:  SW -> NW -> NE -> SE -> SW  */

	univ_xarray[0] =  W - ((W - dlg_coors[SW].utm_e) * multiplier) ;
	univ_yarray[0] =  S - ((S - dlg_coors[SW].utm_n) * multiplier) ;

	univ_xarray[1] =  univ_xarray[0] ;
	univ_yarray[1] =  N + ((dlg_coors[NW].utm_n - N) * multiplier) ;

	univ_xarray[2] =  E +  ((dlg_coors[NE].utm_e - E) * multiplier) ;
	univ_yarray[2] =  univ_yarray[1] ;

	univ_xarray[3] =  univ_xarray[2] ;
	univ_yarray[3] =  univ_yarray[0] ;

	univ_xarray[4] =  univ_xarray[0] ;
	univ_yarray[4] =  univ_yarray[0] ;

	/*  if box larger then map edges, inform calling function  */
	if ( W <= dlg_coors[SW].utm_e  ||
	 	 S <= dlg_coors[SW].utm_n  ||
	     N >= dlg_coors[NW].utm_n  ||
		 E >= dlg_coors[NE].utm_e  )
				return(0) ;
	return (1) ;

}

