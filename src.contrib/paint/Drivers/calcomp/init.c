#include <stdio.h>
#include "P.h"

FILE *yellow_f;
FILE *magenta_f;
FILE *cyan_f;
int  ndots;
int rastermd;
int alphamd;
int isinit=0;
Pinit()
{
    extern Praster();
    extern FILE *tmpfile();

    if (isinit) return;
    isinit = 1;
    rastermd = 0;               /* has'nt entered raster mode*/
    alphamd = 0;                /* has'nt entered alpha mode */
    yellow_f = tmpfile();
    if ( yellow_f == NULL)
    {
        return(ERROR);
    }

    magenta_f = tmpfile();
    if ( magenta_f == NULL)
    {
        return(ERROR);
    }

    cyan_f = tmpfile();
    if ( cyan_f == NULL)
    {
        return(ERROR);
    }
    Praster();
}
