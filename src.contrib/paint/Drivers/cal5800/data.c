/* %W% %G% */
#include "P.h"

Pdata (buf, n)
    unsigned char *buf;
{
	int col;
	int one = 1;
	int neg_3 = (-3);
	float hpix = 0.02;	/* This makes a panel 20.48 in. */
	float wpix = 0.02;
	float zerof = 0.0;

	col = 0;
	while (n-- > 0)
		iarray[col++] = ++(*buf++);
	rasfil_ (&hpix, &wpix, &one, &ncols, iarray);
	plot_ (&hpix, &zerof, &neg_3);

}
