/*  @(#)setup_digitizer.c	1.1  5/4/87  */
#include <stdio.h>


int 
D_setup_driver (char *dev)
{

	if (D_open_serial(dev) == -1)
	{
		fprintf (stdout,"Error opening digitizer\n") ;
		return(-1) ;
	}

	D_digit_init() ;

	return(0) ;
}

