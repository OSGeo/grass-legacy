
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
int idpi_1;
*/
 
/* Subroutine */ int idpdrv_(ndp, xd, yd, zd, ncp, ipc, pd, verbose)
int *ndp;
float *xd, *yd, *zd;
int *ncp, *ipc;
float *pd;
int verbose;
{
    /* System generated locals */
    int i__1, i__2, i__3;

    /* Local variables */
    static int jipc;
    static float dnmx, dnmy, dnmz, nmxx, nmxy, nmyx, nmyy;
    static int jipc0, ic2mn, ncpm1;
    static float dnmxx, dnmxy, dnmyx, dnmyy, x0, y0, z0;
    static int ic1, ic2, ip0;
    static float dx1, dy1, dz1, dx2, dy2, dz2, zx0, zy0;
    static int jpd, ipi;
    static float nmx, nmy, nmz;
    static int jpd0, ncp0, ndp0;
    static float dzx1, dzy1, dzx2, dzy2;

/* THIS SUBROUTINE ESTIMATES PARTIAL DERIVATIVES OF THE FIRST AND */
/* SECOND ORDER AT THE DATA POINTS. */
/* THE INPUT PARAMETERS ARE */
/*     NDP = NUMBER OF DATA POINTS, */
/*     XD,YD,ZD = ARRAYS OF DIMENSION NDP CONTAINING THE X, */
/*           Y, AND Z COORDINATES OF THE DATA POINTS, */
/*     NCP = NUMBER OF ADDITIONAL DATA POINTS USED FOR ESTI- */
/*           MATING PARTIAL DERIVATIVES AT EACH DATA POINT, */
/*     IPC = INTEGER ARRAY OF DIMENSION NCP*NDP CONTAINING */
/*           THE POINT NUMBERS OF NCP DATA POINTS CLOSEST TO */
/*           EACH OF THE NDP DATA POINTS. */
/* THE OUTPUT PARAMETER IS */
/*     PD  = ARRAY OF DIMENSION 5*NDP, WHERE THE ESTIMATED */
/*           ZX, ZY, ZXX, ZXY, AND ZYY VALUES AT THE DATA */
/*           POINTS ARE TO BE STORED. */
/* DECLARATION STATEMENTS */
/* PRELIMINARY PROCESSING */
    /* Parameter adjustments */
    --pd;
    --ipc;
    --zd;
    --yd;
    --xd;

    /* Function Body */
/* L10: */
    ndp0 = *ndp;
    ncp0 = *ncp;
    ncpm1 = ncp0 - 1;
/* ESTIMATION OF ZX AND ZY */
/* L20: */
    i__1 = ndp0;
    for (ip0 = 1; ip0 <= i__1; ++ip0) {
        if (verbose)
          G_clicker();
	x0 = xd[ip0];
	y0 = yd[ip0];
	z0 = zd[ip0];
	nmx = (float)0.;
	nmy = (float)0.;
	nmz = (float)0.;
	jipc0 = ncp0 * (ip0 - 1);
	i__2 = ncpm1;
	for (ic1 = 1; ic1 <= i__2; ++ic1) {
	    jipc = jipc0 + ic1;
	    ipi = ipc[jipc];
	    dx1 = xd[ipi] - x0;
	    dy1 = yd[ipi] - y0;
	    dz1 = zd[ipi] - z0;
	    ic2mn = ic1 + 1;
	    i__3 = ncp0;
	    for (ic2 = ic2mn; ic2 <= i__3; ++ic2) {
		jipc = jipc0 + ic2;
		ipi = ipc[jipc];
		dx2 = xd[ipi] - x0;
		dy2 = yd[ipi] - y0;
		dnmz = dx1 * dy2 - dy1 * dx2;
		if (dnmz == (float)0.) {
		    goto L22;
		}
		dz2 = zd[ipi] - z0;
		dnmx = dy1 * dz2 - dz1 * dy2;
		dnmy = dz1 * dx2 - dx1 * dz2;
		if (dnmz >= (float)0.) {
		    goto L21;
		}
		dnmx = -(double)dnmx;
		dnmy = -(double)dnmy;
		dnmz = -(double)dnmz;
L21:
		nmx += dnmx;
		nmy += dnmy;
		nmz += dnmz;
L22:
		;
	    }
/* L23: */
	}
	jpd0 = ip0 * 5;
	pd[jpd0 - 4] = -(double)nmx / nmz;
	pd[jpd0 - 3] = -(double)nmy / nmz;
/* L24: */
    }
/* ESTIMATION OF ZXX, ZXY, AND ZYY */
/* L30: */
    i__1 = ndp0;
    for (ip0 = 1; ip0 <= i__1; ++ip0) {
	jpd0 += 5;
	x0 = xd[ip0];
	jpd0 = ip0 * 5;
	y0 = yd[ip0];
	zx0 = pd[jpd0 - 4];
	zy0 = pd[jpd0 - 3];
	nmxx = (float)0.;
	nmxy = (float)0.;
	nmyx = (float)0.;
	nmyy = (float)0.;
	nmz = (float)0.;
	jipc0 = ncp0 * (ip0 - 1);
	i__2 = ncpm1;
	for (ic1 = 1; ic1 <= i__2; ++ic1) {
	    jipc = jipc0 + ic1;
	    ipi = ipc[jipc];
	    dx1 = xd[ipi] - x0;
	    dy1 = yd[ipi] - y0;
	    jpd = ipi * 5;
	    dzx1 = pd[jpd - 4] - zx0;
	    dzy1 = pd[jpd - 3] - zy0;
	    ic2mn = ic1 + 1;
	    i__3 = ncp0;
	    for (ic2 = ic2mn; ic2 <= i__3; ++ic2) {
		jipc = jipc0 + ic2;
		ipi = ipc[jipc];
		dx2 = xd[ipi] - x0;
		dy2 = yd[ipi] - y0;
		dnmz = dx1 * dy2 - dy1 * dx2;
		if (dnmz == (float)0.) {
		    goto L32;
		}
		jpd = ipi * 5;
		dzx2 = pd[jpd - 4] - zx0;
		dzy2 = pd[jpd - 3] - zy0;
		dnmxx = dy1 * dzx2 - dzx1 * dy2;
		dnmxy = dzx1 * dx2 - dx1 * dzx2;
		dnmyx = dy1 * dzy2 - dzy1 * dy2;
		dnmyy = dzy1 * dx2 - dx1 * dzy2;
		if (dnmz >= (float)0.) {
		    goto L31;
		}
		dnmxx = -(double)dnmxx;
		dnmxy = -(double)dnmxy;
		dnmyx = -(double)dnmyx;
		dnmyy = -(double)dnmyy;
		dnmz = -(double)dnmz;
L31:
		nmxx += dnmxx;
		nmxy += dnmxy;
		nmyx += dnmyx;
		nmyy += dnmyy;
		nmz += dnmz;
L32:
		;
	    }
/* L33: */
	}
	pd[jpd0 - 2] = -(double)nmxx / nmz;
	pd[jpd0 - 1] = -(double)(nmxy + nmyx) / (nmz * (float)2.);
	pd[jpd0] = -(double)nmyy / nmz;
/* L34: */
    }
    return 0;
} /* idpdrv_ */
