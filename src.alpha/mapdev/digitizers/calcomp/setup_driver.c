
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

