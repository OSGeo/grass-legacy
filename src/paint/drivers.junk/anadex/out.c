#
/********************************************************

NAME:		Pout ()

FUNCTION:	output a buffer

USAGE:		Pout (buf, len)

		char buf[]

PARMS:		buf:	buffer of chars to output
		len:	number of chars to output

NOTES:		
**********************************************************/

#include "P.h"

#undef Pout

Pout (buf, len)
    char *buf ;
{
    while (len-- > 0)
    {
	Poutc (*buf);
	buf++;
    }
}
