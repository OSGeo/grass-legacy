/*  @(#)setup_driver.c	2.1  6/26/87  */
#include "geograph.h"
#include <sgtty.h>
#include <stdio.h>

D_setup_driver(dev)
    char *dev ;
{
	int n ;

	if (D_open_serial(dev) == -1)
	{
		printf("Error opening digitizer\n") ;
			/*  slow it down  */
			for (n=0; n<9999999;  n++)
				;
		exit(-1) ;
	}

	D_digit_init(CPI_800, HOR_RT, VRT_UP, TOP_MOUNT) ;


	return(0) ;
}

