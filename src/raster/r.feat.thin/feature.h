/************************************************************************/
/*** 								      ***/
/***				feature.h			      ***/
/***  Header file for use with r.sfeature - 			      ***/
/***  Jo Wood, ASSIST, Dept of Geography, University of Leicester     ***/
/***  V1.0  - 7th February, 1993				      ***/
/***								      ***/
/************************************************************************/

#include "gis.h" 		/* This MUST be included in all GRASS	*/
				/* programs. It sets up the necessary 	*/
				/* prototypes for GRASS library calls.	*/

#define TRUE 1
#define FALSE 0

#define FLAT ((CELL)0)
#define PIT ((CELL)1)
#define PEAK ((CELL)2)
#define PASS ((CELL)5)

#define NUM_CATS ((CELL)4)


/* ------ Global variables ------ */

#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

char		*dem_in_name,	/* Name of elevation file to process.	*/
		*feat_in_name,	/* Name of feature file to process.	*/
    		*rast_out_name,	/* Name of the raster output file.	*/

		sites,		/* Holds the command line flag (0/1).	*/

    		*mapset1_in,	/* If no problems, these will be names 	*/
		*mapset2_in,	/* of mapsets containing the files to	*/
		*mapset_out,	/* be processed. Otherwise, error code.	*/
		corners;        /* Flag forcing triangulation of corners*/


#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

int		fd_dem,fd_feat,	/* File descriptor for input and	*/
		fd_out,		/* output raster files.			*/
		nrows,ncols,	/* Number of rows and columns in raster.*/
		xbar,ybar,n;	/* Centroid of raster object.		*/
