/*  @(#)set_window.c	2.1  6/26/87  */
#include "digit.h"
#include "dig_head.h"

#ifdef BEEP
#undef BEEP
#endif
#define BEEP  XBell (dpy, 25)

init_window ()
{
    if (Dig_Enabled)
    {
	set_window ();
    }
    else
    {
	window_rout (CM->head.N, CM->head.S, CM->head.E, CM->head.W) ;
    }
}

set_window()
{
	extern Widget toplevel;
	char buff[256] ;
	double Ux1, Uy1 ;
	double Ux2, Uy2 ;
	double N, S, E, W ;
	extern Widget monolog;
	XEvent event;

	if (!Dig_Enabled )
	{
	    set_window_w_mouse ();
	    return (0);
	}
	write_info(1, "Identify corners of graphics window.") ;

        make_monolog	
	  (1,"Locate cursor on one corner of desired window. Then hit any <key>");
	XFlush (dpy);
	XmUpdateDisplay (toplevel);
	
	
	/*get_digitizer_button_xy(&Ux1, &Uy1) ;*/
	while (1)
	{
	XFlush (dpy);
	XmUpdateDisplay (toplevel);
	    if (_coll_a_pnt (&Ux1, &Uy1))
		break;
	    if (XCheckWindowEvent (XtDisplay (monolog), XtWindow (monolog),
		       ButtonPressMask, &event))
		  break;
	}
	BEEP;
	make_monolog
	(1,"Now place cursor on diagonal corner of window. Then hit any  <Key>");
	while (1)
	{
	XFlush (dpy);
	XmUpdateDisplay (toplevel);
	    if (_coll_a_pnt (&Ux2, &Uy2))
		break;
	    if (XCheckWindowEvent (XtDisplay (monolog), XtWindow (monolog),
		       ButtonPressMask, &event))
		  break;
	}
	XFlush (dpy);
	XmUpdateDisplay (toplevel);

	/*get_digitizer_button_xy(&Ux2, &Uy2) ;*/
	
	BEEP;
	end_message();



	N = Uy2 > Uy1 ? Uy2 : Uy1 ;
	S = Uy2 < Uy1 ? Uy2 : Uy1 ;
	E = Ux2 > Ux1 ? Ux2 : Ux1 ;
	W = Ux2 < Ux1 ? Ux2 : Ux1 ;
/*
	window_conversions(N, S, E, W) ;
	*/
	window_rout (N, S, E, W) ;
}
