/* %W% %G% */
#include "P.h"
Prle (buf, n)
    unsigned char *buf;
{
    int col;
    int one = 1;
    int neg_3 = (-3);
    float hpix = 0.02;
    float wpix = 0.02;
    float zerof = 0.0;
    unsigned char value, repeat;

    col = 0;
    while (n-- > 0)
    {
	repeat = *buf++;
	value = (*buf++) + 1;

	while (repeat-- > 0)
	    iarray[col++] = value;
    }
    rasfil_ (&hpix, &wpix, &one, &col, iarray);
    plot_ (&hpix, &zerof, &neg_3);
}
