#include "P.h"
static unsigned char pattern[2][5] =
/*    0%   25%   50%   75%  100%   */
   {0000, 0042, 0146, 0335, 0377,  /* even rows */
    0000, 0210, 0231, 0167, 0377}; /* odd rows */


/*  Created from hdp550c driver by Garth Tier, 
    CSIRO Division of Wildlife and Ecology, 
    Alice Springs NT 0870, Australia
    Email: tie013@its.csiro.au*/

    /* Cannot use BLACK so use composite black  */

dither (col, c, y, m)
{
    unsigned char bit;
    unsigned char *pat;
    int byte;
    int row;

    bit = 1 << (7 -(col & 7));
    byte = col >> 3;

    pat= pattern[ras_row&1];    /* odd/even */
    CYAN[byte]    |= pat[c] & bit;
    YELLOW[byte]  |= pat[y] & bit;
    MAGENTA[byte] |= pat[m] & bit;
}
