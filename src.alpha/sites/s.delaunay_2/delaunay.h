/************************************************************************/
/*** 								      ***/
/***	    			delaunay.h			      ***/
/***  Header file for use with s.delaunay.			      ***/
/***  Jo Wood, ASSIST, Dept of Geography, University of Leicester     ***/
/***  V1.0  - 19th July, 1995 					      ***/
/***								      ***/
/************************************************************************/

#include "gis.h" 	/* This MUST be included in all GRASS programs	*/
			/* It sets up the necessary prototypes for all	*/
			/* GRASS library calls.				*/

#include "Vect.h"	/* Must be included in programs that manipulate	*/
			/* vectors.					*/

#include "dig_head.h"	/* Used for vector header information.		*/


/* ------ Global variables ------ */

#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

char		*sites_in_name,	/* Name of the sites file to process.	*/
    		*vect_out_name,	/* Name of the vector output file.	*/

    		*mapset_in,	/* If no problems, these will be names 	*/
		*mapset_out,	/* of mapsets containing the files to 	*/
				/* be processed. Otherwise, error code.	*/
		vrml;		/* Flag for VRML 1.0 output.		*/

#ifndef MAIN
    extern                      /* Externally defined if not main()     */
#endif

float		zscale;		/* Vertical scaling.		   	*/

#ifndef MAIN
    extern                      /* Externally defined if not main()     */
#endif

FILE		*sites_fptr;	/* Sites file pointer.			*/
