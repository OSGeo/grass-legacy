
#include<stdio.h>
#include<math.h>

#define abs(x) ((x) >= 0 ? (x) : -(x))
#define dabs(x) (double)abs(x)
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define dmin(a,b) (double)min(a,b)
#define dmax(a,b) (double)max(a,b)
 

/* Subroutine */ int idgrid_(xd, yd, nt, ipt, nl, ipl, nxi, nyi, xi, yi, ngp, 
	igp)
float *xd, *yd;
int *nt, *ipt, *nl, *ipl, *nxi, *nyi;
float *xi, *yi;
int *ngp, *igp;
{
    /* System generated locals */
    int i__1, i__2, i__3, i__4;
    float r__1, r__2;

    /* Local variables */
    static int il0t3, insd, it0t3;
    static float ximn, yimn, ximx, yimx;
    static int jigp0, jigp1, jngp0, jngp1, l, ilp1t3, iximn, iximx;
    static float x1, y1, x2, y2, x3, y3;
    static int jigp1i, il0, nl0, ip1, ip2, it0, nxinyi, ip3, nt0, ixi, 
	    iyi;
    static float yii, xii;
    static int izi;
    static float xmn, ymn, xmx, ymx;
    static int ngp0, ngp1, ilp1, nxi0, nyi0;

/* THIS SUBROUTINE ORGANIZES GRID POINTS FOR SURFACE FITTING BY */
/* SORTING THEM IN ASCENDING ORDER OF TRIANGLE NUMBERS AND OF THE */
/* BORDER LINE SEGMENT NUMBER. */
/* THE INPUT PARAMETERS ARE */
/*     XD,YD = ARRAYS OF DIMENSION NDP CONTAINING THE X AND Y */
/*           COORDINATES OF THE DATA POINTS, WHERE NDP IS THE */
/*           NUMBER OF THE DATA POINTS, */
/*     NT  = NUMBER OF TRIANGLES, */
/*     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE */
/*           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES, */
/*     NL  = NUMBER OF BORDER LINE SEGMENTS, */
/*     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE */
/*           POINT NUMBERS OF THE END POINTS OF THE BORDER */
/*           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE */
/*           NUMBERS, */
/*     NXI = NUMBER OF GRID POINTS IN THE X COORDINATE, */
/*     NYI = NUMBER OF GRID POINTS IN THE Y COORDINATE, */
/*     XI,YI = ARRAYS OF DIMENSION NXI AND NYI CONTAINING */
/*           THE X AND Y COORDINATES OF THE GRID POINTS, */
/*           RESPECTIVELY. */
/* THE OUTPUT PARAMETERS ARE */
/*     NGP = INTEGER ARRAY OF DIMENSION 2*(NT+2*NL) WHERE THE */
/*           NUMBER OF GRID POINTS THAT BELONG TO EACH OF THE */
/*           TRIANGLES OR OF THE BORDER LINE SEGMENTS ARE TO */
/*           BE STORED, */
/*     IGP = INTEGER ARRAY OF DIMENSION NXI*NYI WHERE THE */
/*           GRID POINT NUMBERS ARE TO BE STORED IN ASCENDING */
/*           ORDER OF THE TRIANGLE NUMBER AND THE BORDER LINE */
/*           SEGMENT NUMBER. */
/* DECLARATION STATEMENTS */
/* STATEMENT FUNCTIONS */
/* PRELIMINARY PROCESSING */
    /* Parameter adjustments */
    --igp;
    --ngp;
    --yi;
    --xi;
    --ipl;
    --ipt;
    --yd;
    --xd;

    /* Function Body */
    nt0 = *nt;
    nl0 = *nl;
    nxi0 = *nxi;
    nyi0 = *nyi;
    nxinyi = nxi0 * nyi0;
/* Computing MIN */
    r__1 = xi[1], r__2 = xi[nxi0];
    ximn = dmin(r__1,r__2);
/* Computing MAX */
    r__1 = xi[1], r__2 = xi[nxi0];
    ximx = dmax(r__1,r__2);
/* Computing MIN */
    r__1 = yi[1], r__2 = yi[nyi0];
    yimn = dmin(r__1,r__2);
/* Computing MAX */
    r__1 = yi[1], r__2 = yi[nyi0];
    yimx = dmax(r__1,r__2);
/* DETERMINES GRID POINTS INSIDE THE DATA AREA. */
    jngp0 = 0;
    jngp1 = (nt0 + (nl0 << 1) << 1) + 1;
    jigp0 = 0;
    jigp1 = nxinyi + 1;
    i__1 = nt0;
    for (it0 = 1; it0 <= i__1; ++it0) {
	ngp0 = 0;
	ngp1 = 0;
	it0t3 = it0 * 3;
	ip1 = ipt[it0t3 - 2];
	ip2 = ipt[it0t3 - 1];
	ip3 = ipt[it0t3];
	x1 = xd[ip1];
	y1 = yd[ip1];
	x2 = xd[ip2];
	y2 = yd[ip2];
	x3 = xd[ip3];
	y3 = yd[ip3];
/* Computing MIN */
	r__1 = min(x1,x2);
	xmn = dmin(r__1,x3);
/* Computing MAX */
	r__1 = max(x1,x2);
	xmx = dmax(r__1,x3);
/* Computing MIN */
	r__1 = min(y1,y2);
	ymn = dmin(r__1,y3);
/* Computing MAX */
	r__1 = max(y1,y2);
	ymx = dmax(r__1,y3);
	insd = 0;
	i__2 = nxi0;
	for (ixi = 1; ixi <= i__2; ++ixi) {
	    if (xi[ixi] >= xmn && xi[ixi] <= xmx) {
		goto L10;
	    }
	    if (insd == 0) {
		goto L20;
	    }
	    iximx = ixi - 1;
	    goto L30;
L10:
	    if (insd == 1) {
		goto L20;
	    }
	    insd = 1;
	    iximn = ixi;
L20:
	    ;
	}
	if (insd == 0) {
	    goto L150;
	}
	iximx = nxi0;
L30:
	i__2 = nyi0;
	for (iyi = 1; iyi <= i__2; ++iyi) {
	    yii = yi[iyi];
	    if (yii < ymn || yii > ymx) {
		goto L140;
	    }
	    i__3 = iximx;
	    for (ixi = iximn; ixi <= i__3; ++ixi) {
		xii = xi[ixi];
		l = 0;
		if ((r__1 = (x1 - xii) * (y2 - yii) - (y1 - yii) * (x2 - xii))
			 < (float)0.) {
		    goto L130;
		} else if (r__1 == 0) {
		    goto L40;
		} else {
		    goto L50;
		}
L40:
		l = 1;
L50:
		if ((r__1 = (x2 - xii) * (y3 - yii) - (y2 - yii) * (x3 - xii))
			 < (float)0.) {
		    goto L130;
		} else if (r__1 == 0) {
		    goto L60;
		} else {
		    goto L70;
		}
L60:
		l = 1;
L70:
		if ((r__1 = (x3 - xii) * (y1 - yii) - (y3 - yii) * (x1 - xii))
			 < (float)0.) {
		    goto L130;
		} else if (r__1 == 0) {
		    goto L80;
		} else {
		    goto L90;
		}
L80:
		l = 1;
L90:
		izi = nxi0 * (iyi - 1) + ixi;
		if (l == 1) {
		    goto L100;
		}
		++ngp0;
		++jigp0;
		igp[jigp0] = izi;
		goto L130;
L100:
		if (jigp1 > nxinyi) {
		    goto L120;
		}
		i__4 = nxinyi;
		for (jigp1i = jigp1; jigp1i <= i__4; ++jigp1i) {
		    if (izi == igp[jigp1i]) {
			goto L130;
		    }
/* L110: */
		}
L120:
		++ngp1;
		--jigp1;
		igp[jigp1] = izi;
L130:
		;
	    }
L140:
	    ;
	}
L150:
	++jngp0;
	ngp[jngp0] = ngp0;
	--jngp1;
	ngp[jngp1] = ngp1;
/* L160: */
    }
/* DETERMINES GRID POINTS OUTSIDE THE DATA AREA. */
/* - IN SEMI-INFINITE RECTANGULAR AREA. */
    i__1 = nl0;
    for (il0 = 1; il0 <= i__1; ++il0) {
	ngp0 = 0;
	ngp1 = 0;
	il0t3 = il0 * 3;
	ip1 = ipl[il0t3 - 2];
	ip2 = ipl[il0t3 - 1];
	x1 = xd[ip1];
	y1 = yd[ip1];
	x2 = xd[ip2];
	y2 = yd[ip2];
	xmn = ximn;
	xmx = ximx;
	ymn = yimn;
	ymx = yimx;
	if (y2 >= y1) {
	    xmn = dmin(x1,x2);
	}
	if (y2 <= y1) {
	    xmx = dmax(x1,x2);
	}
	if (x2 <= x1) {
	    ymn = dmin(y1,y2);
	}
	if (x2 >= x1) {
	    ymx = dmax(y1,y2);
	}
	insd = 0;
	i__2 = nxi0;
	for (ixi = 1; ixi <= i__2; ++ixi) {
	    if (xi[ixi] >= xmn && xi[ixi] <= xmx) {
		goto L170;
	    }
	    if (insd == 0) {
		goto L180;
	    }
	    iximx = ixi - 1;
	    goto L190;
L170:
	    if (insd == 1) {
		goto L180;
	    }
	    insd = 1;
	    iximn = ixi;
L180:
	    ;
	}
	if (insd == 0) {
	    goto L310;
	}
	iximx = nxi0;
L190:
	i__2 = nyi0;
	for (iyi = 1; iyi <= i__2; ++iyi) {
	    yii = yi[iyi];
	    if (yii < ymn || yii > ymx) {
		goto L300;
	    }
	    i__3 = iximx;
	    for (ixi = iximn; ixi <= i__3; ++ixi) {
		xii = xi[ixi];
		l = 0;
		if ((r__1 = (x1 - xii) * (y2 - yii) - (y1 - yii) * (x2 - xii))
			 < (float)0.) {
		    goto L210;
		} else if (r__1 == 0) {
		    goto L200;
		} else {
		    goto L290;
		}
L200:
		l = 1;
L210:
		if ((r__1 = (x2 - x1) * (xii - x1) + (y2 - y1) * (yii - y1)) <
			 (float)0.) {
		    goto L290;
		} else if (r__1 == 0) {
		    goto L220;
		} else {
		    goto L230;
		}
L220:
		l = 1;
L230:
		if ((r__1 = (x1 - x2) * (xii - x2) + (y1 - y2) * (yii - y2)) <
			 (float)0.) {
		    goto L290;
		} else if (r__1 == 0) {
		    goto L240;
		} else {
		    goto L250;
		}
L240:
		l = 1;
L250:
		izi = nxi0 * (iyi - 1) + ixi;
		if (l == 1) {
		    goto L260;
		}
		++ngp0;
		++jigp0;
		igp[jigp0] = izi;
		goto L290;
L260:
		if (jigp1 > nxinyi) {
		    goto L280;
		}
		i__4 = nxinyi;
		for (jigp1i = jigp1; jigp1i <= i__4; ++jigp1i) {
		    if (izi == igp[jigp1i]) {
			goto L290;
		    }
/* L270: */
		}
L280:
		++ngp1;
		--jigp1;
		igp[jigp1] = izi;
L290:
		;
	    }
L300:
	    ;
	}
L310:
	++jngp0;
	ngp[jngp0] = ngp0;
	--jngp1;
	ngp[jngp1] = ngp1;
/* - IN SEMI-INFINITE TRIANGULAR AREA. */
	ngp0 = 0;
	ngp1 = 0;
	ilp1 = il0 % nl0 + 1;
	ilp1t3 = ilp1 * 3;
	ip3 = ipl[ilp1t3 - 1];
	x3 = xd[ip3];
	y3 = yd[ip3];
	xmn = ximn;
	xmx = ximx;
	ymn = yimn;
	ymx = yimx;
	if (y3 >= y2 && y2 >= y1) {
	    xmn = x2;
	}
	if (y3 <= y2 && y2 <= y1) {
	    xmx = x2;
	}
	if (x3 <= x2 && x2 <= x1) {
	    ymn = y2;
	}
	if (x3 >= x2 && x2 >= x1) {
	    ymx = y2;
	}
	insd = 0;
	i__2 = nxi0;
	for (ixi = 1; ixi <= i__2; ++ixi) {
	    if (xi[ixi] >= xmn && xi[ixi] <= xmx) {
		goto L320;
	    }
	    if (insd == 0) {
		goto L330;
	    }
	    iximx = ixi - 1;
	    goto L340;
L320:
	    if (insd == 1) {
		goto L330;
	    }
	    insd = 1;
	    iximn = ixi;
L330:
	    ;
	}
	if (insd == 0) {
	    goto L440;
	}
	iximx = nxi0;
L340:
	i__2 = nyi0;
	for (iyi = 1; iyi <= i__2; ++iyi) {
	    yii = yi[iyi];
	    if (yii < ymn || yii > ymx) {
		goto L430;
	    }
	    i__3 = iximx;
	    for (ixi = iximn; ixi <= i__3; ++ixi) {
		xii = xi[ixi];
		l = 0;
		if ((r__1 = (x1 - x2) * (xii - x2) + (y1 - y2) * (yii - y2)) <
			 (float)0.) {
		    goto L360;
		} else if (r__1 == 0) {
		    goto L350;
		} else {
		    goto L420;
		}
L350:
		l = 1;
L360:
		if ((r__1 = (x3 - x2) * (xii - x2) + (y3 - y2) * (yii - y2)) <
			 (float)0.) {
		    goto L380;
		} else if (r__1 == 0) {
		    goto L370;
		} else {
		    goto L420;
		}
L370:
		l = 1;
L380:
		izi = nxi0 * (iyi - 1) + ixi;
		if (l == 1) {
		    goto L390;
		}
		++ngp0;
		++jigp0;
		igp[jigp0] = izi;
		goto L420;
L390:
		if (jigp1 > nxinyi) {
		    goto L410;
		}
		i__4 = nxinyi;
		for (jigp1i = jigp1; jigp1i <= i__4; ++jigp1i) {
		    if (izi == igp[jigp1i]) {
			goto L420;
		    }
/* L400: */
		}
L410:
		++ngp1;
		--jigp1;
		igp[jigp1] = izi;
L420:
		;
	    }
L430:
	    ;
	}
L440:
	++jngp0;
	ngp[jngp0] = ngp0;
	--jngp1;
	ngp[jngp1] = ngp1;
/* L450: */
    }
    return 0;
} /* idgrid_ */
