/*  @(#)leave.c	2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "Vect.h"
#include "georef.h"

int leave (void)
{
	return (curses_yes_no(1, "Shall we continue?  ") ) ;
}
