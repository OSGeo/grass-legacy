/************************************************************************/
/*** 								      ***/
/***	   			 spline.h			      ***/
/***  Header file for use with v.spline - 			      ***/
/***  Jo Wood, ASSIST, Dept of Geography, University of Leicester     ***/
/***  V1.2  - 23rd July, 1995					      ***/
/***								      ***/
/************************************************************************/

#include "gis.h" 	/* This MUST be included in all GRASS programs	*/
			/* It sets up the necessary prototypes for all	*/
			/* GRASS library calls.				*/

#include "Vect.h"	/* Must be included in programs that manipulate	*/
			/* vectors.					*/

#include "dig_head.h"	/* Used for vector header information.		*/

#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <math.h>

/* ------ DEFINES ------ */

#define MIN(x,y) ((x)<(y)? (x):(y))
#define MAX(x,y) ((x)>(y)? (x):(y))

/* ------ Global variables ------ */

#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

char		*vect_in_name,	/* Name of the vector file to process.	*/
    		*rast_out_name,	/* Name of the vector output file.	*/
		rooks,		/* Rooks case rasterisation flag.	*/
		truncation,

    		*mapset_in,	/* If no problems, these will be names 	*/
		*mapset_out;	/* of mapsets containing the files to 	*/
				/* be processed. Otherwise, error code.	*/
#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

int		interval,	/* Contour interval for constraining 	*/
				/* the spline function.			*/
		nrows,ncols;	/* Dimensions of raster.		*/
