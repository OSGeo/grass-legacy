/************************************************************************/
/*** 								      ***/
/***				lags.h				      ***/
/***  Header file for use with r.lags - 			      ***/
/***  Jo Wood, ASSIST, Dept of Geography, University of Leicester     ***/
/***  V1.0  - 7th February, 1993				      ***/
/***								      ***/
/************************************************************************/

#include "gis.h" 		/* This MUST be included in all GRASS	*/
				/* programs. It sets up the necessary 	*/
				/* prototypes for GRASS library calls.	*/
#include <math.h>

#define TRUE 1
#define FALSE 0

#define MORAN 1
#define TEXTURAL 2

#define GLEVS 80

#define MIN(x,y) ((x)<(y)? (x):(y))
#define MAX(x,y) ((x)>(y)? (x):(y))

/* ------ Global variables ------ */

#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

char		*rast_in_name,	/* Name of the raster file to process.	*/
    		*rast_out1_name,/* Name of the raster output files.	*/
		rast_out2_name[80],
		con_name[80],
		asmo_name[80],
		ent_name[80],
		asym_name[80],
		idm_name[80],

    		*mapset_in,	/* If no problems, these will be names 	*/
		*mapset_out,	/* of mapsets containing the files to 	*/
				/* be processed. Otherwise, error code.	*/

		measure,	/* Type of spatial dependence measure	*/

		non_zero,	/* Flag that indicates exclusion of	*/
				/* zero values from calculations.	*/
		vario;		/* Flag indicating variogram to be 	*/
				/* plotted.				*/

#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

int		fd_in,		/* File descriptor for input and	*/
		fd1_out,	/* output raster files.			*/
		fd2_out,
		fd_con,
		fd_asmo,
		fd_ent,
		fd_asym,
		fd_ent,
		fd_idm;

#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

double		contrast(),	/* Textural measurements.		*/
		asmoment(),
		entropy(),
		asymmetry(),
		idmoment();
