/* %W% %G% */
/******************************************************************
* percent (n, d, s)
*
*  print a counter to stderr
*  prints percentage of n in d if divisible by s
*
*  example:
*
*       for (row = 0; row < nrows; row++)
*       {
*           percent (row, nrows, 10);
*                ...
*       }
*       percent (row, nrows, 10);
*
*  will print percent complete for row/nrows in multiples of 10  
*****************************************************************/ 
#include <stdio.h>
static prev = -1;
percent (n,d,s)
{
    register int x;

    x = n*100/d ;
    if (x % s) return;
    if (n <= 0 || n >= d || x != prev)
    {
	prev = x;
	fprintf (stderr,"%4d%%\b\b\b\b\b",x);
	fflush (stderr);
    }
    if (x >= 100)
    {
	fprintf (stderr,"\n");
	prev = -1;
    }
}
