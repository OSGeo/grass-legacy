/***********************************************************************

NAME:		V_error()

FUNCTION:	Deals with error messages generated by other Visual ask 
			routines.

USAGE:		V_error(message)

PARAMETERS:
			char *message ;

RETURNS:	zero (0) 

ALGORITHM:	
		|	Print the message to standard error

CALLS:		

***********************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <grass/vask.h>

#ifdef __MINGW32__
unsigned int sleep (unsigned int seconds)
{
    /* TODO: no sleep for now */
    return 0;
}
#endif

int V_error( char *message )
{
	fprintf(stderr,"V_ask error: %s\n", message) ;
	sleep(4) ;

	return 0;
}
