/*  @(#)dig_dev.c	1.1  5/4/87  */

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include "ginput.h"
#include "dig_curses.h"
#include "local_proto.h"
#include "glocale.h"

static int	IORser;

int D_readall (int *Xraw, int *Yraw)
{
   int button;

    button = get_diginput(Xraw, Yraw);			

    return (button);

}


int D_ask_if_err (void)
{
	if ( ! curses_yes_no( 2,_(" Digitizer read error.  Do we continue(y,n)? ")) )
	  close_down(-1) ;
    return 0;
}

int D_get_scale (float *scale)
{
	*scale = digdevice.units_per_inch? 1/digdevice.units_per_inch : 1;
    return 0;
}


int D_end_digit (void)
{
	stop_ginput();
    return 0;
}
