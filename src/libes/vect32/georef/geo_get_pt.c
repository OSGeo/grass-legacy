#include <unistd.h>
#include <stdio.h>
#include "Vect.h"
#include "georef.h"

#define BEEP	   putchar ('\007')

int geo_get_point (double *X, double *Y)
{
	int  ret ;
	int  priority_on ;

	Clear_info() ;
	Clear_base() ;

	get_point_explain() ;

	priority_on = set_priority() ;

	D_clear_driver() ;

	if ( D_cursor_buttons() )
		ret = get_point_buttons( X, Y) ;
	else
		ret = get_point_generic( X, Y) ;

	if ( priority_on == 0)
		unset_priority() ;

	return(ret) ;
}


int 
get_point_explain (void) 
{
	int first_button ;
	char message[128] ;

	Clear_info() ;

	_Write_base( 2, "  Place the digitizer cursor on the point you wish the coordinates for.") ;
	_Write_base( 3, "  Hit the specified key to pick the point.") ;
	_Write_base( 4, "  Keep the cursor on the point until you hear a BEEP") ;
	Write_base( 5, "  When your satisfied with the point..  Accept it.") ;

	if ( D_cursor_buttons() )
	{
		first_button = D_start_button() ;

		sprintf(message, "  USE DIGITIZER:  Key '%d' to pick a point.",
			first_button) ;
		Write_base(7, message) ;
		sprintf(message, "                  Key '%d' to accept chosen point.",
			first_button+1) ;
		Write_base(8, message) ;
		sprintf(message, "                  Key '%d' to abort.",
			first_button+2) ;
		Write_base(9, message) ;

	}
	else
	{
		Write_base(7, "  USE KEYBOARD:  Key 'p' to pick a point.") ;
		Write_base(8, "                 Key 'a' to accept a point.") ;
		Write_base(9, "                 Key 'A' to abort.") ;
	}

	Write_info (3,"      Coordinates:                X - Current - Y         X -  Saved  - Y") ;
	Write_info (4,"         Place digitizer cursor on tablet") ;

	return 0;
}


int 
get_point_buttons (double *X, double *Y)
{
	int button ;
	double   Xmapcoor;
	double   Ymapcoor;
	double   Xsaved ;
	double   Ysaved ;
	char message[128] ;

	Xsaved = 0.0 ;
	Ysaved = 0.0 ;

	button = 0 ;

	for(;;)
	{

		button = _coll_a_pnt ( &Xmapcoor, &Ymapcoor) ;

		sprintf(message,
		"                          %12.2f %12.2f %12.2f %12.2f |",
				Xmapcoor, Ymapcoor, Xsaved, Ysaved);
		Write_info(4, message) ;

		switch(button)
		{
			/*  save point  */
			case 1:
				_coll_a_pnt ( &Xmapcoor, &Ymapcoor) ;
				Xsaved = Xmapcoor ;
				Ysaved = Ymapcoor ;
				BEEP ;
				break ;

			/*  accept point  */
			case 2:
				if (Xsaved == 0.0  &&  Ysaved == 0.0)
				{
					Write_info( 4, " No point was saved. Use abort to leave") ;
					sleep(2) ;
				}
				else
				{
					*X = Xsaved ;
					*Y = Ysaved ;
					return(0) ;
				}
				break ;

			/*  abort  */
			case 3:
				return(-1) ;
				break ;

			default:
				break ;

		}  /*  switch  */
	}

}  /*  check_map_buttons()  */



int 
get_point_generic (double *X, double *Y)
{
	double   Xsaved ;
	double   Ysaved ;
	double   Xmapcoor;
	double   Ymapcoor;

	char message[128] ;

	Xsaved = 0.0 ;
	Ysaved = 0.0 ;

	set_keyboard() ;

	for(;;)
	{

		_coll_a_pnt ( &Xmapcoor, &Ymapcoor) ;
		sprintf(message,
				"                          %12.2f %12.2f %12.2f %12.2f |",
				Xmapcoor, Ymapcoor, Xsaved, Ysaved);
		Write_info(4, message) ;

		/*  no key was hit  */
		if ( key_hit(message) == 0 )
			continue ;

		switch(*message)
		{
			/*  save point  */
			case 'p':
				_coll_a_pnt ( &Xmapcoor, &Ymapcoor) ;
				Xsaved = Xmapcoor ;
				Ysaved = Ymapcoor ;
				BEEP ;
				break ;

			/*  accept point  */
			case 'a':
				if (Xsaved == 0.0  &&  Ysaved == 0.0)
				{
					Write_info( 4, " No point was saved. Use abort to leave") ;
					sleep(2) ;
				}
				else
				{
					*X = Xsaved ;
					*Y = Ysaved ;
					unset_keyboard() ;
					return(0) ;
				}
				break ;

			/*  abort  */
			case 'A':
				unset_keyboard() ;
				return(-1) ;
				break ;

			default:
				break ;

		}

	}

}  

