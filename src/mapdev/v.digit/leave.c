/*  @(#)leave.c	2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "dig_curses.h"
#include "local_proto.h"
#include "glocale.h"

int leave (void)
{
	return (curses_yes_no_default(1, _("Shall we continue? "), 1) ) ;
}
