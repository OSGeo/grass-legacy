/*  @(#)reset_map.c	2.1  6/26/87  */

/*
*  This also sets up the map the first time.
*/

#include	<stdio.h>

reset_map(Map, coor_file)
	struct Map_info *Map;
	char	*coor_file ;
{
	int ok ;
	int priority_on ;

	ok = 0 ;
	while (! ok)
	{
	/*
	 *  Set up map
	 */
		Clear_info() ;
		Clear_base() ;

		if ( init_map(coor_file) < 0)
			close_down() ;

		check_scale (Map) ;

		check_map_explain() ;

		priority_on = set_priority() ;

		D_clear_driver() ;

		if ( D_cursor_buttons() )
			check_map_buttons() ;
		else
			if ( D_foot_switch() )
				check_map_ft_swtch() ;
			else	
				check_map_generic() ;

		if ( priority_on == 0)
			unset_priority() ;

		Clear_info() ;
		Clear_base() ;

		ok = ask_yes_no ("If satisfied with the registration, enter 'y', else 'n'");

		Clear_info() ;
	}

	return(0) ;
}


check_map_explain() 
{
	Write_base( 3, "  This screen is used to verify that the map registration is correct.") ;
	Write_base( 4, "  Check known points on the map with the digitizer and compare them") ;
	Write_base( 5, "  with the coordinates at the bottom of this screen") ;
}


check_map_buttons ()
{
	int button ;
	int first_button ;
	double   Xmapcoor;
	double   Ymapcoor;
	double   Xsaved ;
	double   Ysaved ;
	char message[128] ;

	Clear_info() ;
	Xsaved = 0.0 ;
	Ysaved = 0.0 ;

	first_button = D_start_button() ;

	sprintf(message, "CHECK MAP:   Key '%d' to preserve point;  Any other Key to continue", first_button) ;
	Write_info(1, message) ;
	Write_info (3,"      Coordinates:                X - Current - Y         X -  Saved  - Y") ;
	Write_info (4,"         Place digitizer cursor on tablet") ;

	button = 0 ;

	for(;;)
	{

		button = _coll_a_pnt ( &Xmapcoor, &Ymapcoor) ;

	/****
		sprintf(message,
		"                          %12.2lf %12.2lf %12.2lf %12.2lf |",
				Xmapcoor, Ymapcoor, Xsaved, Ysaved);
	****/
		Write_info(4, message) ;

		sprintf(message,
		"     %d                   %12.2lf %12.2lf %12.2lf %12.2lf |",
			button, Xmapcoor, Ymapcoor, Xsaved, Ysaved);
		Write_info(4, message) ;

		if (button)
		{
			/*  hit a key  */
			if (button == 1 )
			{
				Xsaved = Xmapcoor ;
				Ysaved = Ymapcoor ;
			}
			else
				break ;
		}
	}


}  /*  check_map_buttons()  */



check_map_generic()
{
	double   Xsaved ;
	double   Ysaved ;
	double   Xmapcoor;
	double   Ymapcoor;

	char message[128] ;

	Clear_info() ;
	Xsaved = 0.0 ;
	Ysaved = 0.0 ;

	Write_info(1, "CHECK MAP:   Hit 's' to preserve point;  Any other key to continue") ;
	Write_info (3,"      Coordinates:                X - Current - Y         X -  Saved  - Y") ;
	Write_info (4,"         Place digitizer cursor on tablet") ;

	set_keyboard() ;

	for(;;)
	{

		_coll_a_pnt ( &Xmapcoor, &Ymapcoor) ;
		sprintf(message,
				"                          %12.2lf %12.2lf %12.2lf %12.2lf |",
				Xmapcoor, Ymapcoor, Xsaved, Ysaved);
		Write_info(4, message) ;

		if ( key_hit(message) )
		{
			if (*message == 's')
			{
				Xsaved = Xmapcoor ;
				Ysaved = Ymapcoor ;
			}
			else
				break ;
		}

	}
	unset_keyboard ();

}  /*  check_map_generic()  */

check_map_ft_swtch()
{
	int   n ;
	int   FtswStat ;
	double   Xsaved ;
	double   Ysaved ;
	double   Xmapcoor;
	double   Ymapcoor;

	char	Ftsw_Str[20] ;
	char message[128] ;

	Clear_info() ;
	Xsaved = 0.0 ;
	Ysaved = 0.0 ;

	Write_info(1, "CHECK MAP:   Hit 's' to preserve point;  Any other key to continue") ;
	Write_info (2," Foot |                             Coordinates");
	Write_info (3,"Switch|                           X - Current - Y         X -  Saved  - Y") ;

	n = strncpy (Ftsw_Str, "Up  \0", 5);

	set_keyboard() ;

	for(;;)
	{

		FtswStat = _coll_a_pnt ( &Xmapcoor, &Ymapcoor) ;

		if (FtswStat)
			n = strncpy (Ftsw_Str, "Down\0", 5);
		else
			n = strncpy (Ftsw_Str, "Up  \0", 5);

		sprintf(message, "%6s| %15s  |%12.2lf %12.2lf %12.2lf %12.2lf |",
				Ftsw_Str, "",  Xmapcoor, Ymapcoor, Xsaved, Ysaved);
		Write_info(4, message) ;
		if ( key_hit(message) )
		{
			if (*message == 's')
			{
				Xsaved = Xmapcoor ;
				Ysaved = Ymapcoor ;
			}
			else
				break ;
		}

	}
	unset_keyboard ();
}
