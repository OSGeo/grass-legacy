/******************************************************************
* I_percent (n, d, s)
*
*  print a counter to tty
*  prints percentage of n in d if divisible by s
*
*  example:
*
*       for (row = 0; row < nrows; row++)
*       {
*           I_percent (row, nrows, 10);
*                ...
*       }
*       I_percent (row, nrows, 10);
*
*  will print percent complete for row/nrows in multiples of 10  
*****************************************************************/ 
#include <stdio.h>
#include "imagery.h"

static prev = -1;
int I_percent (int n,int d,int s)
{
    register int x;

    x = n*100/d ;
    if (x % s) return;
    if (n <= 0 || n >= d || x != prev)
    {
	prev = x;
	fprintf (stdout,"%4d%%\b\b\b\b\b",x);
	fflush (stdout);
    }
    if (x >= 100)
    {
	fprintf (stdout,"\n");
	prev = -1;
    }
}
