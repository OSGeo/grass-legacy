/*******************************************************************************
                    Floodplain Analysis Toolkit
               Mother Earth Systems, Boulder, Colorado


This software was been developed for the U.S. Army Corps of Engineers,
Omaha District under contract #DACW45-92-P-1301.

This code is in the public domain.  Permission to use, copy, modify, and
distribute this software and its documentation for any purpose and without
fee is granted.

*******************************************************************************/

#define TRUE  1
#define FALSE 0

#define MAX_PVERTS 256

#define FNAMELEN   257
#define BUFFLEN   1024

/*-----------------------*/
/* structure definitions */
/*-----------------------*/
#define C_HEAD struct Cell_head
#define M_INFO struct Map_info
#define L_PNTS struct line_pnts
#define X_INFO struct xs_info
 
/*-----------------------*/
/* function declarations */
/*-----------------------*/
#include "gis.h"
#include "display.h"
#include "raster.h"
