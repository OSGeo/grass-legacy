/*  @(#)setup_digitizer.c	1.1  5/4/87  */
#include <stdio.h>


D_setup_driver(dev)
    char *dev ;
{

	if (D_open_serial(dev) == -1)
	{
		printf("Error opening digitizer\n") ;
		return(-1) ;
	}

	D_digit_init() ;

	return(0) ;
}

