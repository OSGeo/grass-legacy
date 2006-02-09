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
#include <grass/gis.h>
#include <grass/imagery.h>

int I_percent (int n,int d,int s)
{
    return G_percent(n, d, s);
}
