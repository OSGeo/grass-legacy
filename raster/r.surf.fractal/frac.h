/************************************************************************/
/***				  frac.h			      ***/
/***                    Header file for use with r.frac               ***/
/***		     Jo Wood, V 1.0  - 19th October, 1994	      ***/
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
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

/* fft.c */
int fft(int, double *[2], int, int, int);
/* gauss.c */
float gauss(int);
/* interface.c */
int interface(int, char *[]);
/* max_pow.c */
int max_pow2(int);
/* open_files.c */
int open_files(void);
/* process.c */
int process(void);
int data_reset(double *[2], int);
/* rand1.c */
float rand1(int);
/* spec_syn.c */
int specsyn(double *[2], int);
/* write_rast.c */
int write_rast(double *[2], int, int);
