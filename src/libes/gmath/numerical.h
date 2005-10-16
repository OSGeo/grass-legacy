/*
 *				  numerical.h
 *     Header file for use gauss.c, jacobi.c, ...
 *
 *     Modified: MN 6/2001
 *     Originally: frac.h by Jo Wood, V 1.0  - 19th October, 1994
*/

#include "gis.h"                /* This MUST be included in all GRASS   */
                                /* programs. It sets up the necessary   */
                                /* prototypes for GRASS library calls.  */
#include "string.h"
#include "stdio.h"

#define MAX(a,b) ((a)>(b) ? (a):(b))
#define SWAP(a,b) tempr=(a); (a)=(b); (b) = tempr

#define TWOPI 6.283185307179590 /* Constant value of 2 pi */

/* ------ Global variables ------ */
/* gauss.c */
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


/* jacobi.c */
#define MC 50
extern char outputfile[50];
extern char groupname[50], subgroup[50], signame[50];

/* del2g.c */
 /* fft constants */
#define FORWARD 1
#define INVERSE -1
#define SCALE 1
#define NOSCALE 0

extern double Thresh;
extern int NumOrients;
