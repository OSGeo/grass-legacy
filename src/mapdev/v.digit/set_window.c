/* 
 * $Id$  
 * set_window.c	2.1  6/26/87  */
 
#include "digit.h"
#include "debug.h"
#include "dig_curses.h"
#include "local_proto.h"

#define BEEP putchar ('\007')

int init_window (void)
{
    if (Dig_Enabled)
    {
	set_window_w ();
    }
    else
    {
/*DEBUG*/ debugf ("Seting window to default\n");
	/*
	window_conversions (head.N, head.S, head.E, head.W) ;
	*/
	window_rout (CMap->head.N, CMap->head.S, CMap->head.E, CMap->head.W) ;
    }

    return 0;
}

int 
set_window_w (void)
{
	char buff[256] ;
	double Ux1, Uy1 ;
	double Ux2, Uy2 ;
	double N, S, E, W ;
	int  priority_on ;

	if (!Dig_Enabled)
	{
	    set_window_w_mouse ();
	    return (0);
	}
	Clear_info() ;
	Write_info(1, "Identify corners of graphics window.") ;
	Write_info(2, "  Locate digitizer cursor on one corner of desired window.") ;

	priority_on = set_priority() ;
#ifdef CURSORKEYS
	if( D_cursor_buttons() )
#endif
	{
		Write_info(3, "  Then hit any number <Key>") ;
		ask_driver() ;
	}
#ifdef CURSORKEYS
	else
	{
		Write_info(3, "  Then hit <RETURN>") ;
		Get_curses_text(buff) ;
	}

#endif
	coll_a_pnt ( &Ux1, &Uy1) ;
	BEEP;

	Clear_info() ;
	Write_info(2, "  Now place digitizer cursor on diagonal corner of desired window.") ;
#ifdef CURSORKEYS
	if( D_cursor_buttons() )
#endif
	{
		Write_info(3, "  Then hit any number <Key>") ;
		ask_driver() ;
	}
#ifdef CURSORKEYS
	else
	{
		Write_info(3, "  Then hit <RETURN>") ;
		Get_curses_text(buff) ;
	}

#endif
	coll_a_pnt ( &Ux2, &Uy2) ;
	BEEP;

	if ( priority_on == 0)
		unset_priority() ;

	Clear_info() ;

	N = Uy2 > Uy1 ? Uy2 : Uy1 ;
	S = Uy2 < Uy1 ? Uy2 : Uy1 ;
	E = Ux2 > Ux1 ? Ux2 : Ux1 ;
	W = Ux2 < Ux1 ? Ux2 : Ux1 ;

	/*
	window_conversions(N, S, E, W) ;
	*/
	window_rout (N, S, E, W) ;
    return 0;
}
