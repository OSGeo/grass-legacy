/*  @(#)windows.c	2.1  6/26/87  */
#define MAX_WINDOWS		10

static int c_wind = 0 ;   /* Current window */

static double _west[MAX_WINDOWS]   ;  /*  western edge  (UTM/meters)  */
static double _east[MAX_WINDOWS]   ;  /*  eastern edge  (UTM/meters)  */
static double _south[MAX_WINDOWS]  ;  /*  southern edge (UTM/meters)  */
static double _north[MAX_WINDOWS]  ;  /*  northern edge (UTM/meters)  */

do_window()
{
	char buff[64] ;
	int err ;

	Clear_info() ;
	Clear_base() ;
	Write_base(2, "Window Management") ;
	Write_base(4, "1 - Zoom into smaller window") ;
	Write_base(5, "2 - Zoom out to previous window") ;
	Write_base(6, "3 - return without changing window") ;
	Write_info(2, "> ") ;
	Get_curses_text(buff) ;
	switch(*buff & 0377)
	{
	case '1':
		err = push_window() ;
		break ;
	case '2':
		err = pop_window() ;
		break ;
	default:
		err = -1 ;
		break ;
	}
	Clear_base() ;
	Clear_info() ;
	return(err) ;
}

save_first_window(N, S, E, W)
	double N, S, E, W ;
{
	_west[0] = W ;
	_east[0] = E ;
	_south[0] = S ;
	_north[0] = N ;
}

push_window()
{
	double U_north ;
	double U_south ;
	double U_east ;
	double U_west ;

	Clear_base() ;
	if (c_wind >= MAX_WINDOWS-1)
	{
		Write_info(1, "No more windows") ;
		sleep(2) ;
		Clear_info() ;
		return(-1) ;
	}
	if ( make_window(&U_north, &U_south, &U_east, &U_west) )
		return(-1) ;

	c_wind++ ;
	_west[c_wind] = U_west ;
	_east[c_wind] = U_east ;
	_south[c_wind] = U_south ;
	_north[c_wind] = U_north ;
	window_conversions(U_north, U_south, U_east, U_west) ;
	return(0) ;
}

pop_window()
{
	double U_north ;
	double U_south ;
	double U_east ;
	double U_west ;

	if (c_wind)
	{
		c_wind-- ;
		U_west = _west[c_wind] ;
		U_east = _east[c_wind] ;
		U_south = _south[c_wind] ;
		U_north = _north[c_wind] ;
		window_conversions(U_north, U_south, U_east, U_west) ;
	}

	return(0) ;
}
