
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
 
/* Subroutine */ int idlctn_(ndp, xd, yd, nt, ipt, nl, ipl, xii, yii, iti, 
	iwk, wk)
int *ndp;
float *xd, *yd;
int *nt, *ipt, *nl, *ipl;
float *xii, *yii;
int *iti, *iwk;
float *wk;
{
    /* System generated locals */
    int i__1;
    float r__1, r__2;

    /* Local variables */
    static int idsc[9], il1t3, itsc, it0t3, jiwk, ntsc[9], ntsci, i1, i2, 
	    i3, itipv;
    static float x0, y0, x1, y1, x2, y2, x3, y3, xi, yi;
    static int il1, il2, nl0, ip1, ip2, it0, ip3, nt0;
    static float xs1, xs2, ys1, ys2;
    static int idp, isc, ntl, jwk;
    static float xmn, ymn, xmx, ymx;
    static int ndp0;

/* THIS SUBROUTINE LOCATES A POINT, I.E., DETERMINES TO WHAT TRI- */
/* ANGLE A GIVEN POINT (XII,YII) BELONGS.  WHEN THE GIVEN POINT */
/* DOES NOT LIE INSIDE THE DATA AREA, THIS SUBROUTINE DETERMINES */
/* THE BORDER LINE SEGMENT WHEN THE POINT LIES IN AN OUTSIDE */
/* RECTANGULAR AREA, AND TWO BORDER LINE SEGMENTS WHEN THE POINT */
/* LIES IN AN OUTSIDE TRIANGULAR AREA. */
/* THE INPUT PARAMETERS ARE */
/*     NDP = NUMBER OF DATA POINTS, */
/*     XD,YD = ARRAYS OF DIMENSION NDP CONTAINING THE X AND Y */
/*           COORDINATES OF THE DATA POINTS, */
/*     NT  = NUMBER OF TRIANGLES, */
/*     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE */
/*           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES, */
/*     NL  = NUMBER OF BORDER LINE SEGMENTS, */
/*     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE */
/*           POINT NUMBERS OF THE END POINTS OF THE BORDER */
/*           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE */
/*           NUMBERS, */
/*     XII,YII = X AND Y COORDINATES OF THE POINT TO BE */
/*           LOCATED. */
/* THE OUTPUT PARAMETER IS */
/*     ITI = TRIANGLE NUMBER, WHEN THE POINT IS INSIDE THE */
/*           DATA AREA, OR */
/*           TWO BORDER LINE SEGMENT NUMBERS, IL1 AND IL2, */
/*           CODED TO IL1*(NT+NL)+IL2, WHEN THE POINT IS */
/*           OUTSIDE THE DATA AREA. */
/* THE OTHER PARAMETERS ARE */
/*     IWK = INTEGER ARRAY OF DIMENSION 18*NDP USED INTER- */
/*           NALLY AS A WORK AREA, */
/*     WK  = ARRAY OF DIMENSION 8*NDP USED INTERNALLY AS A */
/*           WORK AREA. */
/* DECLARATION STATEMENTS */
/* STATEMENT FUNCTIONS */
/* PRELIMINARY PROCESSING */
    /* Parameter adjustments */
    --wk;
    --iwk;
    --ipl;
    --ipt;
    --yd;
    --xd;

    /* Function Body */
    ndp0 = *ndp;
    nt0 = *nt;
    nl0 = *nl;
    ntl = nt0 + nl0;
    x0 = *xii;
    y0 = *yii;
/* PROCESSING FOR A NEW SET OF DATA POINTS */
    if (idlc_1 != 0) {
	goto L80;
    }
    idlc_1 = 1;
/* - DIVIDES THE X-Y PLANE INTO NINE RECTANGULAR SECTIONS. */
    xmn = xd[1];
    xmx = xmn;
    ymn = yd[1];
    ymx = ymn;
    i__1 = ndp0;
    for (idp = 2; idp <= i__1; ++idp) {
	xi = xd[idp];
	yi = yd[idp];
	xmn = dmin(xi,xmn);
	xmx = dmax(xi,xmx);
	ymn = dmin(yi,ymn);
	ymx = dmax(yi,ymx);
/* L10: */
    }
    xs1 = (xmn + xmn + xmx) / (float)3.;
    xs2 = (xmn + xmx + xmx) / (float)3.;
    ys1 = (ymn + ymn + ymx) / (float)3.;
    ys2 = (ymn + ymx + ymx) / (float)3.;
/* - DETERMINES AND STORES IN THE IWK ARRAY TRIANGLE NUMBERS OF */
/* - THE TRIANGLES ASSOCIATED WITH EACH OF THE NINE SECTIONS. */
    for (isc = 1; isc <= 9; ++isc) {
	ntsc[isc - 1] = 0;
	idsc[isc - 1] = 0;
/* L20: */
    }
    it0t3 = 0;
    jwk = 0;
    i__1 = nt0;
    for (it0 = 1; it0 <= i__1; ++it0) {
	it0t3 += 3;
	i1 = ipt[it0t3 - 2];
	i2 = ipt[it0t3 - 1];
	i3 = ipt[it0t3];
/* Computing MIN */
	r__1 = xd[i1], r__2 = xd[i2], r__1 = min(r__1,r__2), r__2 = xd[i3];
	xmn = dmin(r__1,r__2);
/* Computing MAX */
	r__1 = xd[i1], r__2 = xd[i2], r__1 = max(r__1,r__2), r__2 = xd[i3];
	xmx = dmax(r__1,r__2);
/* Computing MIN */
	r__1 = yd[i1], r__2 = yd[i2], r__1 = min(r__1,r__2), r__2 = yd[i3];
	ymn = dmin(r__1,r__2);
/* Computing MAX */
	r__1 = yd[i1], r__2 = yd[i2], r__1 = max(r__1,r__2), r__2 = yd[i3];
	ymx = dmax(r__1,r__2);
	if (ymn > ys1) {
	    goto L30;
	}
	if (xmn <= xs1) {
	    idsc[0] = 1;
	}
	if (xmx >= xs1 && xmn <= xs2) {
	    idsc[1] = 1;
	}
	if (xmx >= xs2) {
	    idsc[2] = 1;
	}
L30:
	if (ymx < ys1 || ymn > ys2) {
	    goto L40;
	}
	if (xmn <= xs1) {
	    idsc[3] = 1;
	}
	if (xmx >= xs1 && xmn <= xs2) {
	    idsc[4] = 1;
	}
	if (xmx >= xs2) {
	    idsc[5] = 1;
	}
L40:
	if (ymx < ys2) {
	    goto L50;
	}
	if (xmn <= xs1) {
	    idsc[6] = 1;
	}
	if (xmx >= xs1 && xmn <= xs2) {
	    idsc[7] = 1;
	}
	if (xmx >= xs2) {
	    idsc[8] = 1;
	}
L50:
	for (isc = 1; isc <= 9; ++isc) {
	    if (idsc[isc - 1] == 0) {
		goto L60;
	    }
	    jiwk = ntsc[isc - 1] * 9 + isc;
	    iwk[jiwk] = it0;
	    ++ntsc[isc - 1];
	    idsc[isc - 1] = 0;
L60:
	    ;
	}
/* - STORES IN THE WK ARRAY THE MINIMUM AND MAXIMUM OF THE X AND */
/* - Y COORDINATE VALUES FOR EACH OF THE TRIANGLE. */
	jwk += 4;
	wk[jwk - 3] = xmn;
	wk[jwk - 2] = xmx;
	wk[jwk - 1] = ymn;
	wk[jwk] = ymx;
/* L70: */
    }
    goto L110;
/* CHECKS IF IN THE SAME TRIANGLE AS PREVIOUS. */
L80:
    it0 = itipv;
    if (it0 > nt0) {
	goto L90;
    }
    it0t3 = it0 * 3;
    ip1 = ipt[it0t3 - 2];
    x1 = xd[ip1];
    y1 = yd[ip1];
    ip2 = ipt[it0t3 - 1];
    x2 = xd[ip2];
    y2 = yd[ip2];
    if ((x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0) < (float)0.) {
	goto L110;
    }
    ip3 = ipt[it0t3];
    x3 = xd[ip3];
    y3 = yd[ip3];
    if ((x2 - x0) * (y3 - y0) - (y2 - y0) * (x3 - x0) < (float)0.) {
	goto L110;
    }
    if ((x3 - x0) * (y1 - y0) - (y3 - y0) * (x1 - x0) < (float)0.) {
	goto L110;
    }
    goto L170;
/* CHECKS IF ON THE SAME BORDER LINE SEGMENT. */
L90:
    il1 = it0 / ntl;
    il2 = it0 - il1 * ntl;
    il1t3 = il1 * 3;
    ip1 = ipl[il1t3 - 2];
    x1 = xd[ip1];
    y1 = yd[ip1];
    ip2 = ipl[il1t3 - 1];
    x2 = xd[ip2];
    y2 = yd[ip2];
    if (il2 != il1) {
	goto L100;
    }
    if ((x1 - x2) * (x0 - x2) + (y1 - y2) * (y0 - y2) < (float)0.) {
	goto L110;
    }
    if ((x2 - x1) * (x0 - x1) + (y2 - y1) * (y0 - y1) < (float)0.) {
	goto L110;
    }
    if ((x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0) > (float)0.) {
	goto L110;
    }
    goto L170;
/* CHECKS IF BETWEEN THE SAME TWO BORDER LINE SEGMENTS. */
L100:
    if ((x1 - x2) * (x0 - x2) + (y1 - y2) * (y0 - y2) > (float)0.) {
	goto L110;
    }
    ip3 = ipl[il2 * 3 - 1];
    x3 = xd[ip3];
    y3 = yd[ip3];
    if ((x3 - x2) * (x0 - x2) + (y3 - y2) * (y0 - y2) <= (float)0.) {
	goto L170;
    }
/* LOCATES INSIDE THE DATA AREA. */
/* - DETERMINES THE SECTION IN WHICH THE POINT IN QUESTION LIES. */
L110:
    isc = 1;
    if (x0 >= xs1) {
	++isc;
    }
    if (x0 >= xs2) {
	++isc;
    }
    if (y0 >= ys1) {
	isc += 3;
    }
    if (y0 >= ys2) {
	isc += 3;
    }
/* - SEARCHES THROUGH THE TRIANGLES ASSOCIATED WITH THE SECTION. */
    ntsci = ntsc[isc - 1];
    if (ntsci <= 0) {
	goto L130;
    }
    jiwk = isc - 9;
    i__1 = ntsci;
    for (itsc = 1; itsc <= i__1; ++itsc) {
	jiwk += 9;
	it0 = iwk[jiwk];
	jwk = it0 << 2;
	if (x0 < wk[jwk - 3]) {
	    goto L120;
	}
	if (x0 > wk[jwk - 2]) {
	    goto L120;
	}
	if (y0 < wk[jwk - 1]) {
	    goto L120;
	}
	if (y0 > wk[jwk]) {
	    goto L120;
	}
	it0t3 = it0 * 3;
	ip1 = ipt[it0t3 - 2];
	x1 = xd[ip1];
	y1 = yd[ip1];
	ip2 = ipt[it0t3 - 1];
	x2 = xd[ip2];
	y2 = yd[ip2];
	if ((x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0) < (float)0.) {
	    goto L120;
	}
	ip3 = ipt[it0t3];
	x3 = xd[ip3];
	y3 = yd[ip3];
	if ((x2 - x0) * (y3 - y0) - (y2 - y0) * (x3 - x0) < (float)0.) {
	    goto L120;
	}
	if ((x3 - x0) * (y1 - y0) - (y3 - y0) * (x1 - x0) < (float)0.) {
	    goto L120;
	}
	goto L170;
L120:
	;
    }
/* LOCATES OUTSIDE THE DATA AREA. */
L130:
    i__1 = nl0;
    for (il1 = 1; il1 <= i__1; ++il1) {
	il1t3 = il1 * 3;
	ip1 = ipl[il1t3 - 2];
	x1 = xd[ip1];
	y1 = yd[ip1];
	ip2 = ipl[il1t3 - 1];
	x2 = xd[ip2];
	y2 = yd[ip2];
	if ((x2 - x1) * (x0 - x1) + (y2 - y1) * (y0 - y1) < (float)0.) {
	    goto L150;
	}
	if ((x1 - x2) * (x0 - x2) + (y1 - y2) * (y0 - y2) < (float)0.) {
	    goto L140;
	}
	if ((x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0) > (float)0.) {
	    goto L150;
	}
	il2 = il1;
	goto L160;
L140:
	il2 = il1 % nl0 + 1;
	ip3 = ipl[il2 * 3 - 1];
	x3 = xd[ip3];
	y3 = yd[ip3];
	if ((x3 - x2) * (x0 - x2) + (y3 - y2) * (y0 - y2) <= (float)0.) {
	    goto L160;
	}
L150:
	;
    }
    it0 = 1;
    goto L170;
L160:
    it0 = il1 * ntl + il2;
/* NORMAL EXIT */
L170:
    *iti = it0;
    itipv = it0;
    return 0;
} /* idlctn_ */
