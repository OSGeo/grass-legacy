
#include <stdio.h>

/*  Created from tek4695 driver by Garth Tier, 
    CSIRO Division of Wildlife and Ecology, 
    Alice Springs NT 0870, Australia
    Email: tie013@its.csiro.au*/

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL int nrows, ncols ;
GLOBAL int nbytes ;
GLOBAL int ras_row;
GLOBAL int ras_nrows;
GLOBAL unsigned char BLACK[300];   /* increased to 300 */
GLOBAL unsigned char YELLOW[300];  /* increased to 300 */
GLOBAL unsigned char CYAN[300];    /* increased to 300 */
GLOBAL unsigned char MAGENTA[300]; /* increased to 300 */
GLOBAL int NPIXELS;
