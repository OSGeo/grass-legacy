
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
 
/* Subroutine */ int idsfft_(md, ncp, ndp, xd, yd, zd, nxi, nyi, xi, yi, zi, 
	iwk, wk, verbose)
int *md, *ncp, *ndp;
float *xd, *yd, *zd;
int *nxi, *nyi;
float *xi, *yi, *zi;
int *iwk;
float *wk;
int verbose;
{
    /* Initialized data */

    static int lun = 6;


    /* System generated locals */
    int i__1, i__2;

    /* Builtin functions */
    int s_wsfe(), do_fio(), e_wsfe();

    /* Local variables */
    static int jigp, jngp, nngp, jwipc, jwigp, jwipl, ncppv, ndppv, jwngp,
	     jwiwl, jwipt, jwiwp, nxipv, nyipv, jig0mn, jig1mn, jig0mx, 
	    jig1mx, jwigp0, jwngp0, nl;
    extern /* Subroutine */ int idcldp_();
    static int nt;
    extern /* Subroutine */ int idtang_(), idgrid_(), idpdrv_(), idptip_();
    static int md0, il1, il2, iti, ixi, izi, iyi, ncp0, ndp0, ngp0, ngp1, 
	    nxi0, nyi0;



/* THIS SUBROUTINE PERFORMS SMOOTH SURFACE FITTING WHEN THE PRO- */
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
/*     NXI = NUMBER OF OUTPUT GRID POINTS IN THE X COORDINATE */
/*           (MUST BE 1 OR GREATER), */
/*     NYI = NUMBER OF OUTPUT GRID POINTS IN THE Y COORDINATE */
/*           (MUST BE 1 OR GREATER), */
/*     XI  = ARRAY OF DIMENSION NXI CONTAINING THE X */
/*           COORDINATES OF THE OUTPUT GRID POINTS, */
/*     YI  = ARRAY OF DIMENSION NYI CONTAINING THE Y */
/*           COORDINATES OF THE OUTPUT GRID POINTS. */
/* THE OUTPUT PARAMETER IS */
/*     ZI  = DOUBLY-DIMENSIONED ARRAY OF DIMENSION (NXI,NYI), */
/*           WHERE THE INTERPOLATED Z VALUES AT THE OUTPUT */
/*           GRID POINTS ARE TO BE STORED. */
/* THE OTHER PARAMETERS ARE */
/*     IWK = INTEGER ARRAY OF DIMENSION */
/*              MAX0(31,27+NCP)*NDP+NXI*NYI */
/*           USED INTERNALLY AS A WORK AREA, */
/*     WK  = ARRAY OF DIMENSION 5*NDP USED INTERNALLY AS A */
/*           WORK AREA. */
/* THE VERY FIRST CALL TO THIS SUBROUTINE AND THE CALL WITH A NEW */
/* NCP VALUE, A NEW NDP VALUE, AND/OR NEW CONTENTS OF THE XD AND */
/* YD ARRAYS MUST BE MADE WITH MD=1.  THE CALL WITH MD=2 MUST BE */
/* PRECEDED BY ANOTHER CALL WITH THE SAME NCP AND NDP VALUES AND */
/* WITH THE SAME CONTENTS OF THE XD AND YD ARRAYS.  THE CALL WITH */
/* MD=3 MUST BE PRECEDED BY ANOTHER CALL WITH THE SAME NCP, NDP, */
/* NXI, AND NYI VALUES AND WITH THE SAME CONTENTS OF THE XD, YD, */
/* XI, AND YI ARRAYS.  BETWEEN THE CALL WITH MD=2 OR MD=3 AND ITS */
/* PRECEDING CALL, THE IWK AND WK ARRAYS MUST NOT BE DISTURBED. */
/* USE OF A VALUE BETWEEN 3 AND 5 (INCLUSIVE) FOR NCP IS RECOM- */
/* MENDED UNLESS THERE ARE EVIDENCES THAT DICTATE OTHERWISE. */
/* THE LUN CONSTANT IN THE DATA INITIALIZATION STATEMENT IS THE */
/* LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS, */
/* THEREFORE, SYSTEM DEPENDENT. */
/* THIS SUBROUTINE CALLS THE IDCLDP, IDGRID, IDPDRV, IDPTIP, AND */
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
    nxi0 = *nxi;
    nyi0 = *nyi;
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
    if (nxi0 < 1 || nyi0 < 1) {
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
    iwk[3] = nxi0;
    iwk[4] = nyi0;
    goto L30;
L23:
    nxipv = iwk[3];
    nyipv = iwk[4];
    if (nxi0 != nxipv) {
	goto L90;
    }
    if (nyi0 != nyipv) {
	goto L90;
    }
/* ALLOCATION OF STORAGE AREAS IN THE IWK ARRAY.  (FOR MD=1,2,3) */
L30:
    jwipt = 16;
    jwiwl = ndp0 * 6 + 1;
    jwngp0 = jwiwl - 1;
    jwipl = ndp0 * 24 + 1;
    jwiwp = ndp0 * 30 + 1;
    jwipc = ndp0 * 27 + 1;
/* Computing MAX */
    i__1 = 31, i__2 = ncp0 + 27;
    jwigp0 = max(i__1,i__2) * ndp0;
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
/* SORTS OUTPUT GRID POINTS IN ASCENDING ORDER OF THE TRIANGLE */
/* NUMBER AND THE BORDER LINE SEGMENT NUMBER.  (FOR MD=1,2) */
L60:
    if (md0 == 3) {
	goto L70;
    }
    idgrid_(&xd[1], &yd[1], &nt, &iwk[jwipt], &nl, &iwk[jwipl], &nxi0, &nyi0, 
	    &xi[1], &yi[1], &iwk[jwngp0 + 1], &iwk[jwigp0 + 1]);
/* ESTIMATES PARTIAL DERIVATIVES AT ALL DATA POINTS. */
/* (FOR MD=1,2,3) */
L70:
    idpdrv_(&ndp0, &xd[1], &yd[1], &zd[1], &ncp0, &iwk[jwipc], &wk[1],verbose);
/* INTERPOLATES THE ZI VALUES.  (FOR MD=1,2,3) */
/* L80: */
    idpi_1 = 0;
    jig0mx = 0;
    jig1mn = nxi0 * nyi0 + 1;
    nngp = nt + (nl << 1);
    i__1 = nngp;
    for (jngp = 1; jngp <= i__1; ++jngp) {
	iti = jngp;
	if (jngp <= nt) {
	    goto L81;
	}
	il1 = (jngp - nt + 1) / 2;
	il2 = (jngp - nt + 2) / 2;
	if (il2 > nl) {
	    il2 = 1;
	}
	iti = il1 * (nt + nl) + il2;
L81:
	jwngp = jwngp0 + jngp;
	ngp0 = iwk[jwngp];
	if (ngp0 == 0) {
	    goto L86;
	}
	jig0mn = jig0mx + 1;
	jig0mx += ngp0;
	i__2 = jig0mx;
	for (jigp = jig0mn; jigp <= i__2; ++jigp) {
	    jwigp = jwigp0 + jigp;
	    izi = iwk[jwigp];
	    iyi = (izi - 1) / nxi0 + 1;
	    ixi = izi - nxi0 * (iyi - 1);
	    idptip_(&xd[1], &yd[1], &zd[1], &nt, &iwk[jwipt], &nl, &iwk[jwipl]
		    , &wk[1], &iti, &xi[ixi], &yi[iyi], &zi[izi]);
/* L82: */
	}
L86:
	jwngp = jwngp0 + (nngp << 1) + 1 - jngp;
	ngp1 = iwk[jwngp];
	if (ngp1 == 0) {
	    goto L89;
	}
	jig1mx = jig1mn - 1;
	jig1mn -= ngp1;
	i__2 = jig1mx;
	for (jigp = jig1mn; jigp <= i__2; ++jigp) {
	    jwigp = jwigp0 + jigp;
	    izi = iwk[jwigp];
	    iyi = (izi - 1) / nxi0 + 1;
	    ixi = izi - nxi0 * (iyi - 1);
	    idptip_(&xd[1], &yd[1], &zd[1], &nt, &iwk[jwipt], &nl, &iwk[jwipl]
		    , &wk[1], &iti, &xi[ixi], &yi[iyi], &zi[izi]);
/* L87: */
	}
L89:
	;
    }
    return 0;
/* ERROR EXIT */
L90:
    fprintf(stderr,"***   IMPROPER INPUT PARAMETER VALUE(S)\n");
    fprintf(stderr,"md=%d ncp=%d ndp=%d nxi=%d nyi=%d, %s\n",md0,ncp0,
            ndp0,nxi0,nyi0,"ERROR DETECTED IN ROUTINE   IDSFFT");
    return 0;
} /* idsfft_ */
