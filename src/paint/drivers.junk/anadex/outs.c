#
/********************************************************

NAME:		Pouts ()

FUNCTION:	output a string

USAGE:		Pouts (s)

		char *s;
**********************************************************/

#include "P.h"

Pouts (s)
    char *s ;
{
    while (*s)
	Poutc (*s++);
}
