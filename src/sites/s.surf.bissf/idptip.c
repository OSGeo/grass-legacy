
#include<stdio.h>
#include<math.h>

#define abs(x) ((x) >= 0 ? (x) : -(x))
#define dabs(x) (double)abs(x)
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define dmin(a,b) (double)min(a,b)
#define dmax(a,b) (double)max(a,b)
 

/*
int idlc_1;
*/
int idpi_1;
 
/* Subroutine */ int idptip_(xd, yd, zd, nt, ipt, nl, ipl, pdd, iti, xii, yii,
	 zii)
float *xd, *yd, *zd;
int *nt, *ipt, *nl, *ipl;
float *pdd;
int *iti;
float *xii, *yii, *zii;
{
    /* System generated locals */
    static float equiv_0[1];

    /* Builtin functions */
    double sqrt(), atan2(), cos(), sin();

    /* Local variables */
    static int jpdd, jipl, jipt;
    static float csuv, thus, thsv, thuv, thxu, a, b, c, d;
    static int i;
    static float u, v, x[3], y[3], z[3], g1, h1, h2, h3, g2, p0, p1, p2, p3, 
	    p4;
#define p5 (equiv_0)
    static float x0, y0, aa, ab, bb, ad, bc, cc, cd, dd, ac, p00, ap, bp, cp, 
	    pd[15];
#define p50 (equiv_0)
    static float dp, p10, p01, p20, p11, p02, p30, lu, lv, p40, p03, p04, p05, 
	    p41, p14, p21, p31, p12, p13, p22, zu[3], zv[3], p32, p23, dx, dy;

    static int il1, il2, it0, idp, jpd, kpd;
    static float dlt;
    static int ntl;
    static float zuu[3], zuv[3], zvv[3], act2, bdt2, adbc;

/* THIS SUBROUTINE PERFORMS PUNCTUAL INTERPOLATION OR EXTRAPOLA- */
/* TION, I.E., DETERMINES THE Z VALUE AT A POINT. */
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
    if (it0 == idpi_1) {
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
    x0 = x[0];
    y0 = y[0];
    a = x[1] - x0;
    b = x[2] - x0;
    c = y[1] - y0;
    d = y[2] - y0;
    ad = a * d;
    bc = b * c;
    dlt = ad - bc;
    ap = d / dlt;
    bp = -(double)b / dlt;
    cp = -(double)c / dlt;
    dp = a / dlt;
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
    p00 = z[0];
    p10 = zu[0];
    p01 = zv[0];
    p20 = zuu[0] * (float).5;
    p11 = zuv[0];
    p02 = zvv[0] * (float).5;
    h1 = z[1] - p00 - p10 - p20;
    h2 = zu[1] - p10 - zuu[0];
    h3 = zuu[1] - zuu[0];
    p30 = h1 * (float)10. - h2 * (float)4. + h3 * (float).5;
    p40 = h1 * (float)-15. + h2 * (float)7. - h3;
    *p50 = h1 * (float)6. - h2 * (float)3. + h3 * (float).5;
    h1 = z[2] - p00 - p01 - p02;
    h2 = zv[2] - p01 - zvv[0];
    h3 = zvv[2] - zvv[0];
    p03 = h1 * (float)10. - h2 * (float)4. + h3 * (float).5;
    p04 = h1 * (float)-15. + h2 * (float)7. - h3;
    p05 = h1 * (float)6. - h2 * (float)3. + h3 * (float).5;
    lu = sqrt(aa + cc);
    lv = sqrt(bb + dd);
    thxu = atan2(c, a);
    thuv = atan2(d, b) - thxu;
    csuv = cos(thuv);
    p41 = lv * (float)5. * csuv / lu * *p50;
    p14 = lu * (float)5. * csuv / lv * p05;
    h1 = zv[1] - p01 - p11 - p41;
    h2 = zuv[1] - p11 - p41 * (float)4.;
    p21 = h1 * (float)3. - h2;
    p31 = h1 * (float)-2. + h2;
    h1 = zu[2] - p10 - p11 - p14;
    h2 = zuv[2] - p11 - p14 * (float)4.;
    p12 = h1 * (float)3. - h2;
    p13 = h1 * (float)-2. + h2;
    thus = atan2(d - c, b - a) - thxu;
    thsv = thuv - thus;
    aa = sin(thsv) / lu;
    bb = -(double)cos(thsv) / lu;
    cc = sin(thus) / lv;
    dd = cos(thus) / lv;
    ac = aa * cc;
    ad = aa * dd;
    bc = bb * cc;
    g1 = aa * ac * (bc * (float)3. + ad * (float)2.);
    g2 = cc * ac * (ad * (float)3. + bc * (float)2.);
    h1 = -(double)aa * aa * aa * (aa * (float)5. * bb * *p50 + (bc * (
	    float)4. + ad) * p41) - cc * cc * cc * (cc * (float)5. * dd * p05 
	    + (ad * (float)4. + bc) * p14);
    h2 = zvv[1] * (float).5 - p02 - p12;
    h3 = zuu[2] * (float).5 - p20 - p21;
    p22 = (g1 * h2 + g2 * h3 - h1) / (g1 + g2);
    p32 = h2 - p22;
    p23 = h3 - p22;
    idpi_1 = it0;
/* CONVERTS XII AND YII TO U-V SYSTEM. */
L30:
    dx = *xii - x0;
    dy = *yii - y0;
    u = ap * dx + bp * dy;
    v = cp * dx + dp * dy;
/* EVALUATES THE POLYNOMIAL. */
/* L31: */
    p0 = p00 + v * (p01 + v * (p02 + v * (p03 + v * (p04 + v * p05))));
    p1 = p10 + v * (p11 + v * (p12 + v * (p13 + v * p14)));
    p2 = p20 + v * (p21 + v * (p22 + v * p23));
    p3 = p30 + v * (p31 + v * p32);
    p4 = p40 + v * p41;
    *zii = p0 + u * (p1 + u * (p2 + u * (p3 + u * (p4 + u * *p5))));
    return 0;
/* CALCULATION OF ZII BY EXTRAPOLATION IN THE RECTANGLE. */
/* CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED. */
L40:
    if (it0 == idpi_1) {
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
    x0 = x[0];
    y0 = y[0];
    a = y[1] - y[0];
    b = x[1] - x[0];
    c = -(double)b;
    d = a;
    ad = a * d;
    bc = b * c;
    dlt = ad - bc;
    ap = d / dlt;
    bp = -(double)b / dlt;
    cp = -(double)bp;
    dp = ap;
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
    p00 = z[0];
    p10 = zu[0];
    p01 = zv[0];
    p20 = zuu[0] * (float).5;
    p11 = zuv[0];
    p02 = zvv[0] * (float).5;
    h1 = z[1] - p00 - p01 - p02;
    h2 = zv[1] - p01 - zvv[0];
    h3 = zvv[1] - zvv[0];
    p03 = h1 * (float)10. - h2 * (float)4. + h3 * (float).5;
    p04 = h1 * (float)-15. + h2 * (float)7. - h3;
    p05 = h1 * (float)6. - h2 * (float)3. + h3 * (float).5;
    h1 = zu[1] - p10 - p11;
    h2 = zuv[1] - p11;
    p12 = h1 * (float)3. - h2;
    p13 = h1 * (float)-2. + h2;
    p21 = (float)0.;
    p23 = -(double)zuu[1] + zuu[0];
    p22 = p23 * (float)-1.5;
    idpi_1 = it0;
/* CONVERTS XII AND YII TO U-V SYSTEM. */
L50:
    dx = *xii - x0;
    dy = *yii - y0;
    u = ap * dx + bp * dy;
    v = cp * dx + dp * dy;
/* EVALUATES THE POLYNOMIAL. */
/* L51: */
    p0 = p00 + v * (p01 + v * (p02 + v * (p03 + v * (p04 + v * p05))));
    p1 = p10 + v * (p11 + v * (p12 + v * p13));
    p2 = p20 + v * (p21 + v * (p22 + v * p23));
    *zii = p0 + u * (p1 + u * p2);
    return 0;
/* CALCULATION OF ZII BY EXTRAPOLATION IN THE TRIANGLE. */
/* CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED. */
L60:
    if (it0 == idpi_1) {
	goto L70;
    }
/* LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE VERTEX */
/* OF THE TRIANGLE. */
/* L61: */
    jipl = il2 * 3 - 2;
    idp = ipl[jipl];
    x[0] = xd[idp];
    y[0] = yd[idp];
    z[0] = zd[idp];
    jpdd = (idp - 1) * 5;
    for (kpd = 1; kpd <= 5; ++kpd) {
	++jpdd;
	pd[kpd - 1] = pdd[jpdd];
/* L62: */
    }
/* CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL. */
/* L67: */
    p00 = z[0];
    p10 = pd[0];
    p01 = pd[1];
    p20 = pd[2] * (float).5;
    p11 = pd[3];
    p02 = pd[4] * (float).5;
    idpi_1 = it0;
/* CONVERTS XII AND YII TO U-V SYSTEM. */
L70:
    u = *xii - x[0];
    v = *yii - y[0];
/* EVALUATES THE POLYNOMIAL. */
/* L71: */
    p0 = p00 + v * (p01 + v * p02);
    p1 = p10 + v * p11;
    *zii = p0 + u * (p1 + u * p20);
    return 0;
} /* idptip_ */
