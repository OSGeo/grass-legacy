/********************************************************/
/* NAME:	number					*/
/*							*/
/* FUNCTION:	retrieve int from current record	*/
/*							*/
/* USAGE:	number(start,end)			*/
/*							*/
/* INPUT:	"start":- starting byte position	*/
/*		"end":- ending byte position		*/
/*							*/
/* OUTPUT:	length of record			*/
/********************************************************/
#include "tape.h"
number (start,end)
{
    int n;

    n = 0;
    while (start <= end)
	n = n * 256 + tape.buf[start++];
    return (n);
}
