#
/********************************************************
NAME:		Poutc (c)

FUNCTION:	output 1 character
**********************************************************/
#include "P.h"

Poutc ( c)
    unsigned char c;
{
    if (printer.b >= printer.bufsize )
	Pflush ();

    printer.buf[printer.b++] = c;
}
