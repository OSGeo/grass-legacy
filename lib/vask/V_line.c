/***********************************************************************

NAME:		V_line()

FUNCTION:	Allows calling program to set a line of text for the next 
			call to V_call() 

USAGE:		V_line(linenumber, text)

PARAMETERS:
			int linenumber ;
			char *text ;

RETURNS:	zero (0) on success and negative 1 (-1) on failure

ALGORITHM:	
		|	If the request is not legal return -1
		|	Otherwise, accept the request

CALLS:		

***********************************************************************/

#include "vask.h"
int V_line(
	register int linenumber ,
	register char *text )
{
	if (linenumber >= MAX_LINE || linenumber < 0) 
	{
		V_error("Linenumber out of bounds in call to V_line") ;
		return(-1) ;
	}
	V__.page.line[linenumber] = text ;
	return 0;
}
