#include "P.h"

Pdata (buf,n)
    char *buf;
{
    int i ;

    Poutc (DATA);
    Pout (buf, n);

/* Ppictsize() will adjust ncols to multiple of 8. pad with white */
    for (i = 0; i < padding; i++)
	Poutc (WHITE);
}
