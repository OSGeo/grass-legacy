/************************************************************************/
/***				  frac.h			      ***/
/***                    Header file for use with r.frac               ***/
/***		     Jo Wood, V 1.0  - 19th October, 1994	      ***/
/************************************************************************/

#include "gis.h"                /* This MUST be included in all GRASS   */
                                /* programs. It sets up the necessary   */
                                /* prototypes for GRASS library calls.  */

#define MAX(a,b) ((a)>(b) ? (a):(b))
#define SWAP(a,b) tempr=(a); (a)=(b); (b) = tempr

#define TWOPI 6.283185307179590 /* Constant value of 2 pi */

/* ------ Global variables ------ */

#ifndef MAIN
    extern                      /* Externally defined if not main()     */
#endif


char	*rast_out_name, 	/* Name of the raster output file.	*/
	*mapset_out;
                             
#ifndef MAIN
    extern
#endif                       

int	fd_out,			/* File descriptor of output raster     */
	Steps;			/* Number of intermediate images.	*/

#ifndef MAIN
    extern
#endif                       
double	H;			/* Hausdorff-Besickovitch dimension.	*/

