#include "gis.h"
/******************************************************************
* G_percent (n, d, s)
*
*  print a counter to stderr
*  prints percentage of n in d, in increments of s
*
*  example:
*
*       for (row = 0; row < nrows; row++)
*       {
*           G_percent (row, nrows, 10);
*                ...
*       }
*       G_percent (row, nrows, 10);
*
*  will print percent complete for row/nrows in multiples of 10  
*****************************************************************/ 
#include <stdio.h>

static int prev = -1;

int G_percent (int n,int d,int s)
{
    int x = (d <= 0 || s <= 0)
	? 100
	: 100 * n / d;

    if (n <= 0 || n >= d || x > prev + s)
    {
	prev = x;
	fprintf (stderr,"%4d%%\b\b\b\b\b",x);
    }

    if (x >= 100)
    {
	fprintf (stderr,"\n");
	prev = -1;
    }

    return 0;
}
