/* 526.f -- translated by f2c (version of 17 April 1991  13:07:29).
   You must link the resulting object file with the libraries:
	-lf2c -lm -lc -L/usr/unsup/lib -I/usr/unsup/include   (in that order)
*/

#include<stdio.h>
#include<math.h>

#define abs(x) ((x) >= 0 ? (x) : -(x))
#define dabs(x) (double)abs(x)
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define dmin(a,b) (double)min(a,b)
#define dmax(a,b) (double)max(a,b)
 

int idlc_1;
int idpi_1;
 
int idbvip_(md, ncp, ndp, xd, yd, zd, nip, xi, yi, zi, iwk, wk)
int *md, *ncp, *ndp;
float *xd, *yd, *zd;
int *nip;
float *xi, *yi, *zi;
int *iwk;
float *wk;
{
    /* Initialized data */

    static int lun = 6;


    /* System generated locals */
    int i__1, i__2;

    /* Builtin functions */
    int s_wsfe(), do_fio(), e_wsfe();

    /* Local variables */
    static int jwit, jwit0, jwipc, jwipl, ncppv, ndppv, jwiwk, nippv, 
	    jwipt, jwiwl, jwiwp, nl;
    extern /* Subroutine */ int idcldp_();
    static int nt;
    extern /* Subroutine */ int idtang_(), idlctn_(), idpdrv_(), idptip_();
    static int md0, iip, ncp0, ndp0, nip0;



/* THIS SUBROUTINE PERFORMS BIVARIATE INTERPOLATION WHEN THE PRO- */
/* JECTIONS OF THE DATA POINTS IN THE X-Y PLANE ARE IRREGULARLY */
/* DISTRIBUTED IN THE PLANE. */
/* THE INPUT PARAMETERS ARE */
/*     MD  = MODE OF COMPUTATION (MUST BE 1, 2, OR 3), */
/*         = 1 FOR NEW NCP AND/OR NEW XD-YD, */
/*         = 2 FOR OLD NCP, OLD XD-YD, NEW XI-YI, */
/*         = 3 FOR OLD NCP, OLD XD-YD, OLD XI-YI, */
/*     NCP = NUMBER OF ADDITIONAL DATA POINTS USED FOR ESTI- */
/*           MATING PARTIAL DERIVATIVES AT EACH DATA POINT */
/*           (MUST BE 2 OR GREATER, BUT SMALLER THAN NDP), */
/*     NDP = NUMBER OF DATA POINTS (MUST BE 4 OR GREATER), */
/*     XD  = ARRAY OF DIMENSION NDP CONTAINING THE X */
/*           COORDINATES OF THE DATA POINTS, */
/*     YD  = ARRAY OF DIMENSION NDP CONTAINING THE Y */
/*           COORDINATES OF THE DATA POINTS, */
/*     ZD  = ARRAY OF DIMENSION NDP CONTAINING THE Z */
/*           COORDINATES OF THE DATA POINTS, */
/*     NIP = NUMBER OF OUTPUT POINTS AT WHICH INTERPOLATION */
/*           IS TO BE PERFORMED (MUST BE 1 OR GREATER), */
/*     XI  = ARRAY OF DIMENSION NIP CONTAINING THE X */
/*           COORDINATES OF THE OUTPUT POINTS, */
/*     YI  = ARRAY OF DIMENSION NIP CONTAINING THE Y */
/*           COORDINATES OF THE OUTPUT POINTS. */
/* THE OUTPUT PARAMETER IS */
/*     ZI  = ARRAY OF DIMENSION NIP WHERE INTERPOLATED Z */
/*           VALUES ARE TO BE STORED. */
/* THE OTHER PARAMETERS ARE */
/*     IWK = INTEGER ARRAY OF DIMENSION */
/*              MAX0(31,27+NCP)*NDP+NIP */
/*           USED INTERNALLY AS A WORK AREA, */
/*     WK  = ARRAY OF DIMENSION 8*NDP USED INTERNALLY AS A */
/*           WORK AREA. */
/* THE VERY FIRST CALL TO THIS SUBROUTINE AND THE CALL WITH A NEW */
/* NCP VALUE, A NEW NDP VALUE, AND/OR NEW CONTENTS OF THE XD AND */
/* YD ARRAYS MUST BE MADE WITH MD=1.  THE CALL WITH MD=2 MUST BE */
/* PRECEDED BY ANOTHER CALL WITH THE SAME NCP AND NDP VALUES AND */
/* WITH THE SAME CONTENTS OF THE XD AND YD ARRAYS.  THE CALL WITH */
/* MD=3 MUST BE PRECEDED BY ANOTHER CALL WITH THE SAME NCP, NDP, */
/* AND NIP VALUES AND WITH THE SAME CONTENTS OF THE XD, YD, XI, */
/* AND YI ARRAYS.  BETWEEN THE CALL WITH MD=2 OR MD=3 AND ITS */
/* PRECEDING CALL, THE IWK AND WK ARRAYS MUST NOT BE DISTURBED. */
/* USE OF A VALUE BETWEEN 3 AND 5 (INCLUSIVE) FOR NCP IS RECOM- */
/* MENDED UNLESS THERE ARE EVIDENCES THAT DICTATE OTHERWISE. */
/* THE LUN CONSTANT IN THE DATA INITIALIZATION STATEMENT IS THE */
/* LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS, */
/* THEREFORE, SYSTEM DEPENDENT. */
/* THIS SUBROUTINE CALLS THE IDCLDP, IDLCTN, IDPDRV, IDPTIP, AND */
/* IDTANG SUBROUTINES. */
/* DECLARATION STATEMENTS */
    /* Parameter adjustments */
    --wk;
    --iwk;
    --zi;
    --yi;
    --xi;
    --zd;
    --yd;
    --xd;

    /* Function Body */
/* SETTING OF SOME INPUT PARAMETERS TO LOCAL VARIABLES. */
/* (FOR MD=1,2,3) */
/* L10: */
    md0 = *md;
    ncp0 = *ncp;
    ndp0 = *ndp;
    nip0 = *nip;
/* ERROR CHECK.  (FOR MD=1,2,3) */
/* L20: */
    if (md0 < 1 || md0 > 3) {
	goto L90;
    }
    if (ncp0 < 2 || ncp0 >= ndp0) {
	goto L90;
    }
    if (ndp0 < 4) {
	goto L90;
    }
    if (nip0 < 1) {
	goto L90;
    }
    if (md0 >= 2) {
	goto L21;
    }
    iwk[1] = ncp0;
    iwk[2] = ndp0;
    goto L22;
L21:
    ncppv = iwk[1];
    ndppv = iwk[2];
    if (ncp0 != ncppv) {
	goto L90;
    }
    if (ndp0 != ndppv) {
	goto L90;
    }
L22:
    if (md0 >= 3) {
	goto L23;
    }
    iwk[3] = *nip;
    goto L30;
L23:
    nippv = iwk[3];
    if (nip0 != nippv) {
	goto L90;
    }
/* ALLOCATION OF STORAGE AREAS IN THE IWK ARRAY.  (FOR MD=1,2,3) */
L30:
    jwipt = 16;
    jwiwl = ndp0 * 6 + 1;
    jwiwk = jwiwl;
    jwipl = ndp0 * 24 + 1;
    jwiwp = ndp0 * 30 + 1;
    jwipc = ndp0 * 27 + 1;
/* Computing MAX */
    i__1 = 31, i__2 = ncp0 + 27;
    jwit0 = max(i__1,i__2) * ndp0;
/* TRIANGULATES THE X-Y PLANE.  (FOR MD=1) */
/* L40: */
    if (md0 > 1) {
	goto L50;
    }
    idtang_(&ndp0, &xd[1], &yd[1], &nt, &iwk[jwipt], &nl, &iwk[jwipl], &iwk[
	    jwiwl], &iwk[jwiwp], &wk[1]);
    iwk[5] = nt;
    iwk[6] = nl;
    if (nt == 0) {
	return 0;
    }
/* DETERMINES NCP POINTS CLOSEST TO EACH DATA POINT.  (FOR MD=1) */
L50:
    if (md0 > 1) {
	goto L60;
    }
    idcldp_(&ndp0, &xd[1], &yd[1], &ncp0, &iwk[jwipc]);
    if (iwk[jwipc] == 0) {
	return 0;
    }
/* LOCATES ALL POINTS AT WHICH INTERPOLATION IS TO BE PERFORMED. */
/* (FOR MD=1,2) */
L60:
    if (md0 == 3) {
	goto L70;
    }
    idlc_1 = 0;
    jwit = jwit0;
    i__1 = nip0;
    for (iip = 1; iip <= i__1; ++iip) {
	++jwit;
	idlctn_(&ndp0, &xd[1], &yd[1], &nt, &iwk[jwipt], &nl, &iwk[jwipl], &
		xi[iip], &yi[iip], &iwk[jwit], &iwk[jwiwk], &wk[1]);
/* L61: */
    }
/* ESTIMATES PARTIAL DERIVATIVES AT ALL DATA POINTS. */
/* (FOR MD=1,2,3) */
L70:
    idpdrv_(&ndp0, &xd[1], &yd[1], &zd[1], &ncp0, &iwk[jwipc], &wk[1]);
/* INTERPOLATES THE ZI VALUES.  (FOR MD=1,2,3) */
/* L80: */
    idpi_1 = 0;
    jwit = jwit0;
    i__1 = nip0;
    for (iip = 1; iip <= i__1; ++iip) {
	++jwit;
	idptip_(&xd[1], &yd[1], &zd[1], &nt, &iwk[jwipt], &nl, &iwk[jwipl], &
		wk[1], &iwk[jwit], &xi[iip], &yi[iip], &zi[iip]);
/* L81: */
    }
    return 0;
/* ERROR EXIT */
L90:
    fprintf(stderr,"*** IMPROPER INPUT PARAMETER VALUE(S)\n");
    fprintf(stderr,"md=%d, ncp=%d ndp=%d nip=%d\n",md0,ncp0,ndp0,ndp0);
    fprintf(stderr,"ERROR DETECTED IN function IDBVIP\n");
    return 0;
} /* idbvip_ */
