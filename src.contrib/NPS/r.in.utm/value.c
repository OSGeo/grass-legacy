/* %W% %G% */
#include <stdio.h>

value (data, n, sflag)
    register unsigned char *data;
    int n;
    int sflag;
{
    register int v, *z;
    int negative;

    if (negative = (sflag && (*data & 0200)))
    {
	v = *data++ & 0177;
    }
    else
    {
	v = *data++;
    }

    while (--n > 0)
	v = v * 256 + *data++;
/*
fprintf(stderr, "What!!!  V= %d\n", v);
    return negative ? -v : v;
*/
    return v;
}
