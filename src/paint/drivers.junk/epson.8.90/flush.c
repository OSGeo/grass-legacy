#
/********************************************************

NAME:		Pflush ()

FUNCTION:	flush printer buffer
**********************************************************/

#include "P.h"

Pflush ()
{
#ifndef USE_TERMIO
    _Pwait ();
#endif

    if (printer.b > 0)
    {
    if (write (printer.fd, printer.buf, printer.b) != printer.b)
	error ("error writing to printer",1);
    }
    printer.b = 0;
}
