#
/********************************************************

NAME:		Pflush ()

FUNCTION:	flush printer buffer
**********************************************************/

#include "P.h"

Pflush ()
{
#ifndef SYSV
    _Pwait ();
#endif

    if (printer.b > 0)
    {
	if (write (printer.fd, printer.buf, printer.b) != printer.b)
	    error ("error writing to printer");
    }
    printer.b = 0;
}
