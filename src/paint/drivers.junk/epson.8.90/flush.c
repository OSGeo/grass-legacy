#
/********************************************************

NAME:		Pflush ()

FUNCTION:	flush printer buffer
**********************************************************/

#include "P.h"
#include "config.h"

Pflush ()
{
#ifndef HAVE_TERMIO_H
    _Pwait ();
#endif

    if (printer.b > 0)
    {
    if (write (printer.fd, printer.buf, printer.b) != printer.b)
	error ("error writing to printer",1);
    }
    printer.b = 0;
}
