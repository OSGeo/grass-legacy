/*  @(#)dig_dev.c	1.1  5/4/87  */

#include <stdio.h>

static float Scale ;

D_digit_init()
{
	Scale = .001;
	return(1) ;
}


D_readall ()
{
	return(1) ;
}

D_ask_if_err()
{
	if ( ! G_yes( " Digitizer read error. Do we continue(y,n)? ") )
	close_down(-1) ;
}

D_get_scale(scale)
	float *scale ;
{
	*scale = Scale ;
}

D_end_digit ()
{
}

D_open_failed()
{
	return(1) ;
}

D_flush()
{
}

D_open_serial ()
{
	return (0);
}

