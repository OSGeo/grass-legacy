/*  @(#)dig_dev.c	1.1  5/4/87  */

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include "ginput.h"




static int	IORser;


D_readall (Xraw, Yraw)
	int     *Xraw;
	int     *Yraw;
{
   int button;

    button = get_diginput(Xraw, Yraw);			

    return (button);

}


D_ask_if_err()
{
	if ( ! curses_yes_no( 2," Digitizer read error.Do we continue(y,n)? ") )
	  close_down(-1) ;
}

D_get_scale(scale)
	float *scale ;
{
	*scale = digdevice.units_per_inch? 1/digdevice.units_per_inch : 1;
}


D_end_digit ()
{
	stop_ginput();
}
