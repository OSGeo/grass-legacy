/************************************************************************/
/*** 								      ***/
/***			  	moran.hq			      ***/
/***  Header file for use with r.moran - 			      ***/
/***  Jo Wood, ASSIST, Dept of Geography, University of Leicester     ***/
/***  V1.0  - 7th February, 1993				      ***/
/***								      ***/
/************************************************************************/

#include "gis.h" 		/* This MUST be included in all GRASS	*/
				/* programs. It sets up the necessary 	*/
				/* prototypes for GRASS library calls.	*/

#define EDGE ((wsize-1)/2)	/* Number of rows/cols that make up the	*/
				/* 'blank' edge around raster.		*/
#define MAX_WSIZE 21		/* Maximum dimensions of window.	*/

#define TRUE 1
#define FALSE 0

#define NUM_CATS 201


/* ------ Global variables ------ */

#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

char		*rast_in_name,	/* Name of the raster file to process.	*/
    		*rast_out_name,	/* Name of the raster output file.	*/

    		*mapset_in,	/* If no problems, these will be names 	*/
		*mapset_out,	/* of mapsets containing the files to 	*/
				/* be processed. Otherwise, error code.	*/
		NoZero,		/* Flag indicating if zeros processed.	*/
		Brief;		/* Flag indicating if brief output only.*/
#ifndef MAIN
    extern
#endif

int		fd_in,		/* File descriptor for input and	*/
		fd_out,		/* output raster files.			*/
		wsize;		/* Size of local processing window.	*/

#ifndef MAIN
    extern
#endif

CELL		resoln;		/* Planimetric resolution.		*/

