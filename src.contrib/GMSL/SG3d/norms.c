/*  norms.c
    Bill Brown, USACERL
    functions for calculating normals
*/
#include "externs.h"
#include <math.h>
#include <stdio.h>


#define NTOP 0x00001000
#define NBOT 0x00000100
#define NLFT 0x00000010
#define NRGT 0x00000001

#define NALL 0x00001111

#define NTL  0x00001010
#define NTR  0x00001001
#define NBL  0x00000110
#define NBR  0x00000101

static long slice;
static float x_res_z2, y_res_z2;
static float c_z2, c_z2_sq;  
static unsigned long *Norm;
static int Xmod, Ymod;
#ifdef USE_SHORT
    static   short           *Elev;
#else
#ifdef USE_CHAR
    static   unsigned char   *Elev;
#else
#ifdef FCELL_TYPE
    static    float           *Elev;
#else
    static    int             *Elev;
#endif
#endif
#endif



/* OPTIMIZED for constant dy & dx
 * The norm array is always the same size, but diff resolutions
 * force resampled data points to have their normals recalculated,
 * then only those norms are passed to n3f during drawing.
 * Norms are converted to a packed unsigned int for storage,
 * must be converted back at time of use.
 * TODO: fix to correctly calculate norms when mapped to sphere!
 */

/* Uses the previous and next cells (when available) for normal 
calculations to produce smoother normals */
void
recalc_normals(xmod, ymod, elev, norm, zscale)
        int             xmod, ymod;
#ifdef USE_SHORT
        short           *elev;
#else
#ifdef USE_CHAR
        unsigned char   *elev;
#else
#ifdef FCELL_TYPE
        float           *elev;
#else
        int             *elev;
#endif
#endif
#endif
        unsigned int    *norm;
        float           zscale;
{
    int row, col;
    int xcnt, ycnt;
    unsigned nehb;


	xcnt = (int)((X_Size - 1)/xmod);
	ycnt = (int)((Y_Size - 1)/ymod);
/*	
	xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
	ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);
*/
	
	c_z2 = 2.0 * X_Res * Y_Res * xmod * ymod;
	c_z2_sq = c_z2 * c_z2;
	x_res_z2 = 2.0 * X_Res * zscale * xmod;
	y_res_z2 = 2.0 * Y_Res * zscale * ymod;
	slice = ymod * X_Size;
	Xmod = xmod;  
	Ymod = ymod;  
	Norm = norm;
	Elev = elev;

	/* first row - just use single cell */
	/* first col - use bottom & right neighbors */
	fprintf(stderr,"\nrecalculating normals...");
        calc_norm( 0, 0, NBR);
	for (col = 1; col < xcnt ; col++) {
	   /* turn off top neighbor for first row */
	   calc_norm( 0, col*xmod, ~NTOP);
	}    
	/* use bottom & left neighbors for last col */
        calc_norm( 0, col*xmod, NBL);

        /* now use four neighboring points for rows 1 - (n-1) */
	for (row = 1; row < ycnt ; row++) {
	    if(!(row%100)){
		fprintf(stderr,"%d ", row);
	    }
	    /* turn off left neighbor for first col */
	    calc_norm( row*ymod, 0, ~NLFT);

	    /* use all 4 neighbors until last col */
	    for (col = 1; col < xcnt ; col++) {
		calc_norm( row*ymod, col*xmod, NALL);
	    }    

	    /* turn off right neighbor for last col */
	    calc_norm( row*ymod, col*xmod, ~NRGT);
	}
	fprintf(stderr,"\n");

	/* last row */
	/* use top & right neighbors for first col */
	calc_norm( row*ymod, 0, NTR);
	for (col = 1; col < xcnt ; col++) {
	    /* turn off bottom neighbor for last row */
	    calc_norm( row*ymod, col*xmod, ~NBOT);
	}    
	/* use top & left neighbors for last column */
	calc_norm( row*ymod, col*xmod, NTL);
   
}



/****************************************************************/
/* need either four neighbors or two non-linear neighbors */
/* passed initial state of neighbors known from array position */
/* and data row & col */

#define SET_NORM(i) \
       dz1 = z1 - z2;   \
       dz2 = z3 - z4;   \
       temp[0] = (float) -dz1 * y_res_z2;   \
       temp[1] = (float) dz2 * x_res_z2;   \
       temp[2] = c_z2;   \
       normalizer = sqrt(temp[0] * temp[0] + temp[1] * temp[1] + c_z2_sq);    \
       if(!normalizer) normalizer= 1.0;   \
       temp[X] /= normalizer;   \
       temp[Y] /= normalizer;   \
       temp[Z] /= normalizer;   \
       PNORM(i,temp);

calc_norm(drow, dcol, neighbors)
int drow, dcol;
unsigned neighbors;
{
long noffset;
float temp[3], normalizer, dz1, dz2, z0, z1, z2, z3, z4;

    noffset = drow*X_Size + dcol;

    if(Has_null){ /* need to check masked neighbors */
	if(EMBNULL(Elev[noffset]))  /* masked */
	    return(0);
	if(neighbors & NTOP)
	    if(EMBNULL(Elev[ noffset - slice ]))  /* masked */
		neighbors &= ~NTOP;
	if(neighbors & NBOT)
	    if(EMBNULL(Elev[ noffset + slice ]))  /* masked */
		neighbors &= ~NBOT;
	if(neighbors & NLFT)
	    if(EMBNULL(Elev[ noffset - Xmod ]))  /* masked */
		neighbors &= ~NLFT;
	if(neighbors & NRGT)
	    if(EMBNULL(Elev[ noffset + Xmod ]))  /* masked */
		neighbors &= ~NRGT;
    }
    if(Anozero->val){ /* need to check masked neighbors */
	if(neighbors & NTOP)
	    if(!Elev[ noffset - slice ])  /* masked */
		neighbors &= ~NTOP;
	if(neighbors & NBOT)
	    if(!Elev[ noffset + slice ])  /* masked */
		neighbors &= ~NBOT;
	if(neighbors & NLFT)
	    if(!Elev[ noffset - Xmod ])  /* masked */
		neighbors &= ~NLFT;
	if(neighbors & NRGT)
	    if(!Elev[ noffset + Xmod ])  /* masked */
		neighbors &= ~NRGT;
    }
    if(!neighbors) return(0);  /* none */
    
    z1 = z2 = z3 = z4 = z0 = Elev[noffset];

    if(neighbors & NRGT){
	z1 = Elev[noffset+Xmod];
	if(!(neighbors & NLFT))
	   z2 = z0 + (z0 - z1);
    }
    if(neighbors & NLFT){
	z2 = Elev[noffset-Xmod];
	if(!(neighbors & NRGT))
	   z1 = z0 + (z0 - z2);
    }
    if(neighbors & NTOP){
        z4 = Elev[noffset-slice];
	if(!(neighbors & NBOT))
	   z3 = z0 + (z0 - z4);
    }
    if(neighbors & NBOT){
        z3 = Elev[noffset+slice];
	if(!(neighbors & NTOP))
	   z4 = z0 + (z0 - z3);
    }

    SET_NORM(Norm[noffset]);
    return(1);

}



