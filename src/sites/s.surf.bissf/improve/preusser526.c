/* preusser526.f -- translated by f2c (version of 17 April 1991  13:07:29).
   You must link the resulting object file with the libraries:
	-lf2c -lm -lc -L/usr/unsup/lib -I/usr/unsup/include   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer itpv;
    real x0, y0, ap, bp, cp, dp, p00, p10, p20, p30, p40, p50, p01, p11, p21, 
	    p31, p41, p02, p12, p22, p32, p03, p13, p23, p04, p14, p05;
} idpi_;

#define idpi_1 idpi_

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int idptip_(xd, yd, zd, nt, ipt, nl, ipl, pdd, iti, xii, yii,
	 zii)
real *xd, *yd, *zd;
integer *nt, *ipt, *nl, *ipl;
real *pdd;
integer *iti;
real *xii, *yii, *zii;
{
    /* Local variables */
    static integer jpdd, jipl, jipt;
    static real lusq, lvsq, spuv, a, b, c, d;
    static integer i;
    static real u, v, x[3], y[3], z[3], e1, e2, g1, h1, h2, h3, g2, p0, p1, 
	    p2, p3, p4;
#define p5 ((real *)&idpi_1 + 12)
    static real z0, aa, ab, bb, ad, bc, cc, cd, dd, pd[15], zu[3], zv[3], dx, 
	    dy;
    static integer il1, il2, it0, idp, jpd, kpd;
    static real dlt;
    static integer ntl;
    static real zuu[3], zuv[3], zvv[3], act2, bdt2, adbc;

/* THIS SUBROUTINE PERFORMS PUNCTUAL INTERPOLATION OR EXTRAPOLA- */
/* TION, I.E., DETERMINES THE Z VALUE AT A POINT. */
/* ------- */
/* -------   REVISIONS INCLUDED ACCORDING TO REMARKS ON ALG.526 ARE */
/* ------- */
/* -------   1) ACM TOMS, VOL. 5, NO. 2 (JUNE 1979) BY H. AKIMA. */
/* -------   RETENTION OF LOCAL VARIABLES ENFORCED BY EXTENSION */
/* -------   OF COMMON BLOCK IDPI. SOME OTHER MINOR CHANGES FOR */
/* -------   THE SAME REASON. */
/* ------- */
/* -------   2) DEC. 1984 */
/* -------   IMPROVED FORMULAS FOR THE COMPUTATION OF THE POLYNOMIAL */
/* -------   COEFFICIENTS FOR THE INTERPOLATION INSIDE THE TRIANGLE */
/* -------   ACCORDING TO A SUGGESTION BY               A. PREUSSER. */
/* ------- */
/* THE INPUT PARAMETERS ARE */
/*     XD,YD,ZD = ARRAYS OF DIMENSION NDP CONTAINING THE X, */
/*           Y, AND Z COORDINATES OF THE DATA POINTS, WHERE */
/*           NDP IS THE NUMBER OF THE DATA POINTS, */
/*     NT  = NUMBER OF TRIANGLES, */
/*     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE */
/*           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES, */
/*     NL  = NUMBER OF BORDER LINE SEGMENTS, */
/*     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE */
/*           POINT NUMBERS OF THE END POINTS OF THE BORDER */
/*           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE */
/*           NUMBERS, */
/*     PDD = ARRAY OF DIMENSION 5*NDP CONTAINING THE PARTIAL */
/*           DERIVATIVES AT THE DATA POINTS, */
/*     ITI = TRIANGLE NUMBER OF THE TRIANGLE IN WHICH LIES */
/*           THE POINT FOR WHICH INTERPOLATION IS TO BE */
/*           PERFORMED, */
/*     XII,YII = X AND Y COORDINATES OF THE POINT FOR WHICH */
/*           INTERPOLATION IS TO BE PERFORMED. */
/* THE OUTPUT PARAMETER IS */
/*     ZII = INTERPOLATED Z VALUE. */
/* DECLARATION STATEMENTS */
/* PRELIMINARY PROCESSING */
    /* Parameter adjustments */
    --pdd;
    --ipl;
    --ipt;
    --zd;
    --yd;
    --xd;

    /* Function Body */
/* L10: */
    it0 = *iti;
    ntl = *nt + *nl;
    if (it0 <= ntl) {
	goto L20;
    }
    il1 = it0 / ntl;
    il2 = it0 - il1 * ntl;
    if (il1 == il2) {
	goto L40;
    }
    goto L60;
/* CALCULATION OF ZII BY INTERPOLATION. */
/* CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED. */
L20:
    if (it0 == idpi_1.itpv) {
	goto L30;
    }
/* LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE */
/* VERTEXES. */
/* L21: */
    jipt = (it0 - 1) * 3;
    jpd = 0;
    for (i = 1; i <= 3; ++i) {
	++jipt;
	idp = ipt[jipt];
	x[i - 1] = xd[idp];
	y[i - 1] = yd[idp];
	z[i - 1] = zd[idp];
	jpdd = (idp - 1) * 5;
	for (kpd = 1; kpd <= 5; ++kpd) {
	    ++jpd;
	    ++jpdd;
	    pd[jpd - 1] = pdd[jpdd];
/* L22: */
	}
/* L23: */
    }
/* DETERMINES THE COEFFICIENTS FOR THE COORDINATE SYSTEM */
/* TRANSFORMATION FROM THE X-Y SYSTEM TO THE U-V SYSTEM */
/* AND VICE VERSA. */
/* L24: */
    idpi_1.x0 = x[0];
    idpi_1.y0 = y[0];
    a = x[1] - idpi_1.x0;
    b = x[2] - idpi_1.x0;
    c = y[1] - idpi_1.y0;
    d = y[2] - idpi_1.y0;
    ad = a * d;
    bc = b * c;
    dlt = ad - bc;
    idpi_1.ap = d / dlt;
    idpi_1.bp = -(doublereal)b / dlt;
    idpi_1.cp = -(doublereal)c / dlt;
    idpi_1.dp = a / dlt;
/* CONVERTS THE PARTIAL DERIVATIVES AT THE VERTEXES OF THE */
/* TRIANGLE FOR THE U-V COORDINATE SYSTEM. */
/* L25: */
    aa = a * a;
    act2 = a * (float)2. * c;
    cc = c * c;
    ab = a * b;
    adbc = ad + bc;
    cd = c * d;
    bb = b * b;
    bdt2 = b * (float)2. * d;
    dd = d * d;
    for (i = 1; i <= 3; ++i) {
	jpd = i * 5;
	zu[i - 1] = a * pd[jpd - 5] + c * pd[jpd - 4];
	zv[i - 1] = b * pd[jpd - 5] + d * pd[jpd - 4];
	zuu[i - 1] = aa * pd[jpd - 3] + act2 * pd[jpd - 2] + cc * pd[jpd - 1];

	zuv[i - 1] = ab * pd[jpd - 3] + adbc * pd[jpd - 2] + cd * pd[jpd - 1];

	zvv[i - 1] = bb * pd[jpd - 3] + bdt2 * pd[jpd - 2] + dd * pd[jpd - 1];

/* L26: */
    }
/* CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL. */
/* L27: */
    idpi_1.p00 = z[0];
    idpi_1.p10 = zu[0];
    idpi_1.p01 = zv[0];
    idpi_1.p20 = zuu[0] * (float).5;
    idpi_1.p11 = zuv[0];
    idpi_1.p02 = zvv[0] * (float).5;
    h1 = z[1] - idpi_1.p00 - idpi_1.p10 - idpi_1.p20;
    h2 = zu[1] - idpi_1.p10 - zuu[0];
    h3 = zuu[1] - zuu[0];
    idpi_1.p30 = h1 * (float)10. - h2 * (float)4. + h3 * (float).5;
    idpi_1.p40 = h1 * (float)-15. + h2 * (float)7. - h3;
    idpi_1.p50 = h1 * (float)6. - h2 * (float)3. + h3 * (float).5;
    h1 = z[2] - idpi_1.p00 - idpi_1.p01 - idpi_1.p02;
    h2 = zv[2] - idpi_1.p01 - zvv[0];
    h3 = zvv[2] - zvv[0];
    idpi_1.p03 = h1 * (float)10. - h2 * (float)4. + h3 * (float).5;
    idpi_1.p04 = h1 * (float)-15. + h2 * (float)7. - h3;
    idpi_1.p05 = h1 * (float)6. - h2 * (float)3. + h3 * (float).5;
    lusq = aa + cc;
    lvsq = bb + dd;
    spuv = ab + cd;
    idpi_1.p41 = spuv * (float)5. / lusq * idpi_1.p50;
    idpi_1.p14 = spuv * (float)5. / lvsq * idpi_1.p05;
    h1 = zv[1] - idpi_1.p01 - idpi_1.p11 - idpi_1.p41;
    h2 = zuv[1] - idpi_1.p11 - idpi_1.p41 * (float)4.;
    idpi_1.p21 = h1 * (float)3. - h2;
    idpi_1.p31 = h1 * (float)-2. + h2;
    h1 = zu[2] - idpi_1.p10 - idpi_1.p11 - idpi_1.p14;
    h2 = zuv[2] - idpi_1.p11 - idpi_1.p14 * (float)4.;
    idpi_1.p12 = h1 * (float)3. - h2;
    idpi_1.p13 = h1 * (float)-2. + h2;
    e1 = (lvsq - spuv) / (lvsq - spuv + (lusq - spuv));
    e2 = (float)1. - e1;
    g1 = e1 * (float)5. - (float)2.;
    g2 = (float)1. - g1;
    h1 = (e1 * (idpi_1.p50 - idpi_1.p41) + e2 * (idpi_1.p05 - idpi_1.p14)) * (
	    float)5. + (idpi_1.p41 + idpi_1.p14);
    h2 = zvv[1] * (float).5 - idpi_1.p02 - idpi_1.p12;
    h3 = zuu[2] * (float).5 - idpi_1.p20 - idpi_1.p21;
    idpi_1.p22 = h1 + g1 * h2 + g2 * h3;
    idpi_1.p32 = h2 - idpi_1.p22;
    idpi_1.p23 = h3 - idpi_1.p22;
    idpi_1.itpv = it0;
/* CONVERTS XII AND YII TO U-V SYSTEM. */
L30:
    dx = *xii - idpi_1.x0;
    dy = *yii - idpi_1.y0;
    u = idpi_1.ap * dx + idpi_1.bp * dy;
    v = idpi_1.cp * dx + idpi_1.dp * dy;
/* EVALUATES THE POLYNOMIAL. */
/* L31: */
    p0 = idpi_1.p00 + v * (idpi_1.p01 + v * (idpi_1.p02 + v * (idpi_1.p03 + v 
	    * (idpi_1.p04 + v * idpi_1.p05))));
    p1 = idpi_1.p10 + v * (idpi_1.p11 + v * (idpi_1.p12 + v * (idpi_1.p13 + v 
	    * idpi_1.p14)));
    p2 = idpi_1.p20 + v * (idpi_1.p21 + v * (idpi_1.p22 + v * idpi_1.p23));
    p3 = idpi_1.p30 + v * (idpi_1.p31 + v * idpi_1.p32);
    p4 = idpi_1.p40 + v * idpi_1.p41;
    *zii = p0 + u * (p1 + u * (p2 + u * (p3 + u * (p4 + u * *p5))));
    return 0;
/* CALCULATION OF ZII BY EXTRAPOLATION IN THE RECTANGLE. */
/* CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED. */
L40:
    if (it0 == idpi_1.itpv) {
	goto L50;
    }
/* LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE END */
/* POINTS OF THE BORDER LINE SEGMENT. */
/* L41: */
    jipl = (il1 - 1) * 3;
    jpd = 0;
    for (i = 1; i <= 2; ++i) {
	++jipl;
	idp = ipl[jipl];
	x[i - 1] = xd[idp];
	y[i - 1] = yd[idp];
	z[i - 1] = zd[idp];
	jpdd = (idp - 1) * 5;
	for (kpd = 1; kpd <= 5; ++kpd) {
	    ++jpd;
	    ++jpdd;
	    pd[jpd - 1] = pdd[jpdd];
/* L42: */
	}
/* L43: */
    }
/* DETERMINES THE COEFFICIENTS FOR THE COORDINATE SYSTEM */
/* TRANSFORMATION FROM THE X-Y SYSTEM TO THE U-V SYSTEM */
/* AND VICE VERSA. */
/* L44: */
    idpi_1.x0 = x[0];
    idpi_1.y0 = y[0];
    a = y[1] - y[0];
    b = x[1] - x[0];
    c = -(doublereal)b;
    d = a;
    ad = a * d;
    bc = b * c;
    dlt = ad - bc;
    idpi_1.ap = d / dlt;
    idpi_1.bp = -(doublereal)b / dlt;
    idpi_1.cp = -(doublereal)idpi_1.bp;
    idpi_1.dp = idpi_1.ap;
/* CONVERTS THE PARTIAL DERIVATIVES AT THE END POINTS OF THE */
/* BORDER LINE SEGMENT FOR THE U-V COORDINATE SYSTEM. */
/* L45: */
    aa = a * a;
    act2 = a * (float)2. * c;
    cc = c * c;
    ab = a * b;
    adbc = ad + bc;
    cd = c * d;
    bb = b * b;
    bdt2 = b * (float)2. * d;
    dd = d * d;
    for (i = 1; i <= 2; ++i) {
	jpd = i * 5;
	zu[i - 1] = a * pd[jpd - 5] + c * pd[jpd - 4];
	zv[i - 1] = b * pd[jpd - 5] + d * pd[jpd - 4];
	zuu[i - 1] = aa * pd[jpd - 3] + act2 * pd[jpd - 2] + cc * pd[jpd - 1];

	zuv[i - 1] = ab * pd[jpd - 3] + adbc * pd[jpd - 2] + cd * pd[jpd - 1];

	zvv[i - 1] = bb * pd[jpd - 3] + bdt2 * pd[jpd - 2] + dd * pd[jpd - 1];

/* L46: */
    }
/* CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL. */
/* L47: */
    idpi_1.p00 = z[0];
    idpi_1.p10 = zu[0];
    idpi_1.p01 = zv[0];
    idpi_1.p20 = zuu[0] * (float).5;
    idpi_1.p11 = zuv[0];
    idpi_1.p02 = zvv[0] * (float).5;
    h1 = z[1] - idpi_1.p00 - idpi_1.p01 - idpi_1.p02;
    h2 = zv[1] - idpi_1.p01 - zvv[0];
    h3 = zvv[1] - zvv[0];
    idpi_1.p03 = h1 * (float)10. - h2 * (float)4. + h3 * (float).5;
    idpi_1.p04 = h1 * (float)-15. + h2 * (float)7. - h3;
    idpi_1.p05 = h1 * (float)6. - h2 * (float)3. + h3 * (float).5;
    h1 = zu[1] - idpi_1.p10 - idpi_1.p11;
    h2 = zuv[1] - idpi_1.p11;
    idpi_1.p12 = h1 * (float)3. - h2;
    idpi_1.p13 = h1 * (float)-2. + h2;
    idpi_1.p21 = (float)0.;
    idpi_1.p23 = -(doublereal)zuu[1] + zuu[0];
    idpi_1.p22 = idpi_1.p23 * (float)-1.5;
    idpi_1.itpv = it0;
/* CONVERTS XII AND YII TO U-V SYSTEM. */
L50:
    dx = *xii - idpi_1.x0;
    dy = *yii - idpi_1.y0;
    u = idpi_1.ap * dx + idpi_1.bp * dy;
    v = idpi_1.cp * dx + idpi_1.dp * dy;
/* EVALUATES THE POLYNOMIAL. */
/* L51: */
    p0 = idpi_1.p00 + v * (idpi_1.p01 + v * (idpi_1.p02 + v * (idpi_1.p03 + v 
	    * (idpi_1.p04 + v * idpi_1.p05))));
    p1 = idpi_1.p10 + v * (idpi_1.p11 + v * (idpi_1.p12 + v * idpi_1.p13));
    p2 = idpi_1.p20 + v * (idpi_1.p21 + v * (idpi_1.p22 + v * idpi_1.p23));
    *zii = p0 + u * (p1 + u * p2);
    return 0;
/* CALCULATION OF ZII BY EXTRAPOLATION IN THE TRIANGLE. */
/* CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED. */
L60:
    if (it0 == idpi_1.itpv) {
	goto L70;
    }
/* LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE VERTEX */
/* OF THE TRIANGLE. */
/* L61: */
    jipl = il2 * 3 - 2;
    idp = ipl[jipl];
    idpi_1.x0 = xd[idp];
    idpi_1.y0 = yd[idp];
    z0 = zd[idp];
    jpdd = (idp - 1) * 5;
    for (kpd = 1; kpd <= 5; ++kpd) {
	++jpdd;
	pd[kpd - 1] = pdd[jpdd];
/* L62: */
    }
/* CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL. */
/* L67: */
    idpi_1.p00 = z0;
    idpi_1.p10 = pd[0];
    idpi_1.p01 = pd[1];
    idpi_1.p20 = pd[2] * (float).5;
    idpi_1.p11 = pd[3];
    idpi_1.p02 = pd[4] * (float).5;
    idpi_1.itpv = it0;
/* CONVERTS XII AND YII TO U-V SYSTEM. */
L70:
    u = *xii - idpi_1.x0;
    v = *yii - idpi_1.y0;
/* EVALUATES THE POLYNOMIAL. */
/* L71: */
    p0 = idpi_1.p00 + v * (idpi_1.p01 + v * idpi_1.p02);
    p1 = idpi_1.p10 + v * idpi_1.p11;
    *zii = p0 + u * (p1 + u * idpi_1.p20);
    return 0;
} /* idptip_ */

#undef p5


/* Subroutine */ int idcldp_(ndp, xd, yd, ncp, ipc)
integer *ndp;
real *xd, *yd;
integer *ncp, *ipc;
{
    /* Initialized data */

    static integer ncpmx = 25;
    static integer lun = 6;

    /* Format strings */
    static char fmt_2090[] = "(1x/\002 ***   IMPROPER INPUT PARAMETER VALUE(\
S).\002)";
    static char fmt_2091[] = "(1x/\002 ***   ALL COLLINEAR DATA POINTS.\002)";

    static char fmt_2092[] = "(\002   NDP =\002,i5,5x,\002NCP =\002,i5/\002 \
ERROR DETECTED IN ROUTINE   IDCLDP\002/)";

    /* System generated locals */
    integer i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    integer s_wsfe(), e_wsfe(), do_fio();

    /* Local variables */
    static real dsqi;
    static integer ip2mn, ip3mn, nclpt;
    static real dsqmn;
    static integer j1;
    static real dsqmx;
    static integer j3, j4, j2;
    static real x1, y1;
    static integer ip1, ip2, ip3;
    static real dx12, dy12, dx13, dy13;
    static integer jmx, ipc0[25], ncp0, ndp0;
    static real dsq0[25];

    /* Fortran I/O blocks */
    static cilist io___84 = { 0, 0, 0, fmt_2090, 0 };
    static cilist io___85 = { 0, 0, 0, fmt_2091, 0 };
    static cilist io___86 = { 0, 0, 0, fmt_2092, 0 };


/* THIS SUBROUTINE SELECTS SEVERAL DATA POINTS THAT ARE CLOSEST */
/* TO EACH OF THE DATA POINT. */
/* THE INPUT PARAMETERS ARE */
/*     NDP = NUMBER OF DATA POINTS, */
/*     XD,YD = ARRAYS OF DIMENSION NDP CONTAINING THE X AND Y */
/*           COORDINATES OF THE DATA POINTS, */
/*     NCP = NUMBER OF DATA POINTS CLOSEST TO EACH DATA */
/*           POINTS. */
/* THE OUTPUT PARAMETER IS */
/*     IPC = INTEGER ARRAY OF DIMENSION NCP*NDP, WHERE THE */
/*           POINT NUMBERS OF NCP DATA POINTS CLOSEST TO */
/*           EACH OF THE NDP DATA POINTS ARE TO BE STORED. */
/* THIS SUBROUTINE ARBITRARILY SETS A RESTRICTION THAT NCP MUST */
/* NOT EXCEED 25. */
/* THE LUN CONSTANT IN THE DATA INITIALIZATION STATEMENT IS THE */
/* LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS, */
/* THEREFORE, SYSTEM DEPENDENT. */
/* DECLARATION STATEMENTS */
    /* Parameter adjustments */
    --ipc;
    --yd;
    --xd;

    /* Function Body */
/* STATEMENT FUNCTION */
/* PRELIMINARY PROCESSING */
/* L10: */
    ndp0 = *ndp;
    ncp0 = *ncp;
    if (ndp0 < 2) {
	goto L90;
    }
    if (ncp0 < 1 || ncp0 > ncpmx || ncp0 >= ndp0) {
	goto L90;
    }
/* CALCULATION */
/* L20: */
    i__1 = ndp0;
    for (ip1 = 1; ip1 <= i__1; ++ip1) {
/* - SELECTS NCP POINTS. */
	x1 = xd[ip1];
	y1 = yd[ip1];
	j1 = 0;
	dsqmx = (float)0.;
	i__2 = ndp0;
	for (ip2 = 1; ip2 <= i__2; ++ip2) {
	    if (ip2 == ip1) {
		goto L22;
	    }
/* Computing 2nd power */
	    r__1 = xd[ip2] - x1;
/* Computing 2nd power */
	    r__2 = yd[ip2] - y1;
	    dsqi = r__1 * r__1 + r__2 * r__2;
	    ++j1;
	    dsq0[j1 - 1] = dsqi;
	    ipc0[j1 - 1] = ip2;
	    if (dsqi <= dsqmx) {
		goto L21;
	    }
	    dsqmx = dsqi;
	    jmx = j1;
L21:
	    if (j1 >= ncp0) {
		goto L23;
	    }
L22:
	    ;
	}
L23:
	ip2mn = ip2 + 1;
	if (ip2mn > ndp0) {
	    goto L30;
	}
	i__2 = ndp0;
	for (ip2 = ip2mn; ip2 <= i__2; ++ip2) {
	    if (ip2 == ip1) {
		goto L25;
	    }
/* Computing 2nd power */
	    r__1 = xd[ip2] - x1;
/* Computing 2nd power */
	    r__2 = yd[ip2] - y1;
	    dsqi = r__1 * r__1 + r__2 * r__2;
	    if (dsqi >= dsqmx) {
		goto L25;
	    }
	    dsq0[jmx - 1] = dsqi;
	    ipc0[jmx - 1] = ip2;
	    dsqmx = (float)0.;
	    i__3 = ncp0;
	    for (j1 = 1; j1 <= i__3; ++j1) {
		if (dsq0[j1 - 1] <= dsqmx) {
		    goto L24;
		}
		dsqmx = dsq0[j1 - 1];
		jmx = j1;
L24:
		;
	    }
L25:
	    ;
	}
/* - CHECKS IF ALL THE NCP+1 POINTS ARE COLLINEAR. */
L30:
	ip2 = ipc0[0];
	dx12 = xd[ip2] - x1;
	dy12 = yd[ip2] - y1;
	i__2 = ncp0;
	for (j3 = 2; j3 <= i__2; ++j3) {
	    ip3 = ipc0[j3 - 1];
	    dx13 = xd[ip3] - x1;
	    dy13 = yd[ip3] - y1;
/* ----- MODIFICATION BY A. PREUSSER TO AVOID SINGULAR CONSTELLATI
ON */
/* Computing 2nd power */
	    r__1 = dy13 * dx12 - dx13 * dy12;
	    if (r__1 * r__1 / (dsq0[0] * dsq0[j3 - 1]) > (float).06698) {
		goto L50;
	    }
/* ----- 0.06698 CORRESPONDS TO AN ANGLE OF ABOUT 15. DEGREES(=SIN
**2) */
/* L31: */
	}
/* - SEARCHES FOR THE CLOSEST NONCOLLINEAR POINT. */
/* L40: */
	nclpt = 0;
	i__2 = ndp0;
	for (ip3 = 1; ip3 <= i__2; ++ip3) {
	    if (ip3 == ip1) {
		goto L43;
	    }
	    i__3 = ncp0;
	    for (j4 = 1; j4 <= i__3; ++j4) {
		if (ip3 == ipc0[j4 - 1]) {
		    goto L43;
		}
/* L41: */
	    }
	    dx13 = xd[ip3] - x1;
	    dy13 = yd[ip3] - y1;
/* ----- MODIFICATION BY A. PREUSSER TO AVOID SINGULAR CONSTELLATI
ON */
/* Computing 2nd power */
	    r__1 = xd[ip3] - x1;
/* Computing 2nd power */
	    r__2 = yd[ip3] - y1;
	    dsqi = r__1 * r__1 + r__2 * r__2;
/* Computing 2nd power */
	    r__1 = dy13 * dx12 - dx13 * dy12;
	    if (r__1 * r__1 / (dsq0[0] * dsqi) <= (float).06698) {
		goto L43;
	    }
	    if (nclpt == 0) {
		goto L42;
	    }
	    if (dsqi >= dsqmn) {
		goto L43;
	    }
L42:
	    nclpt = 1;
	    dsqmn = dsqi;
	    ip3mn = ip3;
L43:
	    ;
	}
	if (nclpt == 0) {
	    goto L91;
	}
	dsqmx = dsqmn;
	ipc0[jmx - 1] = ip3mn;
/* - REPLACES THE LOCAL ARRAY FOR THE OUTPUT ARRAY. */
L50:
	j1 = (ip1 - 1) * ncp0;
	i__2 = ncp0;
	for (j2 = 1; j2 <= i__2; ++j2) {
	    ++j1;
	    ipc[j1] = ipc0[j2 - 1];
/* L51: */
	}
/* L59: */
    }
    return 0;
/* ERROR EXIT */
L90:
    io___84.ciunit = lun;
    s_wsfe(&io___84);
    e_wsfe();
    goto L92;
L91:
    io___85.ciunit = lun;
    s_wsfe(&io___85);
    e_wsfe();
L92:
    io___86.ciunit = lun;
    s_wsfe(&io___86);
    do_fio(&c__1, (char *)&ndp0, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&ncp0, (ftnlen)sizeof(integer));
    e_wsfe();
    ipc[1] = 0;
    return 0;
/* FORMAT STATEMENTS FOR ERROR MESSAGES */
} /* idcldp_ */

