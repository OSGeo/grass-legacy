/* General projections header file */
#ifndef __PROJECTS_H
#define __PROJECTS_H

#ifndef lint
static char PROJECTS_H_ID[] = "@(#)$Id$";
#endif
    /* standard inclusions */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

	/* some useful constants */
#define HALFPI		1.5707963267948966
#define FORTPI		0.78539816339744833
#define PI		3.14159265358979323846
#define TWOPI		6.2831853071795864769
#define RAD_TO_DEG	57.29577951308232
#define DEG_TO_RAD	.0174532925199432958

typedef struct { double u, v; }	UV;

#ifndef __PJ_LIB
#define XY UV
#define LP UV
#else
typedef struct { double x, y; }     XY;
typedef struct { double lam, phi; } LP;
#endif

typedef union { double  f; int  i; char *s; } PVALUE;

struct PJ_LIST {
	char	*id;		/* projection keyword */
	void	*(*proj)();
	char	*name;		/* basic projection full name */
};
struct PJ_ELLPS {
	char	*id;	/* ellipse keyword name */
	char	*major;	/* a= value */
	char	*ell;	/* elliptical parameter */
	char	*name;	/* comments */
};
typedef struct { double p_x_l, p_y_p, p_x_p, p_y_l; } DERIVS;
struct FACTORS {
	double	h, k;		/* meridinal/parallel scale factors */
	double	omega_2;	/* angular distortion */
	double	s;			/* area scale factor */
	double	a, b;		/* maximum, minimum scale factors */
	double	fact_h;
};
    /* parameter list struct */
typedef struct ARG_list { struct ARG_list *next; char param[1]; } paralist;
	/* base projection data structure */
typedef struct PJconsts {
	XY  (*fwd)(LP, struct PJconsts *);
	LP  (*inv)(XY, struct PJconsts *);
	paralist *params;   /* parameter list */
	void (*pfree)(struct PJconsts *);
	int over;   /* over-range flag */
	int geoc;   /* geocentric latitude flag */
	double
		a,  /* major axis or radius if es==0 */
		e,  /* eccentricity */
		es, /* e ^ 2 */
		ra, /* 1/A */
		one_es, /* 1 - e^2 */
		rone_es, /* 1/one_es */
		lam0, phi0, /* central longitude, latitude */
		x0, y0; /* easting and northing */
#ifdef __PROJ_PARMS
__PROJ_PARMS
#endif /* end of optional extensions */
} PJ;

#ifndef _PJ_FACTORS
extern
#endif
struct FACTORS *pj_sfactors;

#ifndef __PJ_LIST
extern struct PJ_LIST pj_list[];
#endif
#ifndef __PJ_ELLPS
extern struct PJ_ELLPS pj_ellps[];
#endif

#ifdef __PJ_LIB
    /* repeatative projection code */
#define ENTRY(name) PJ *name(PJ *P)
#define E_ERROR { freeup(P); return(0); }
#define F_ERROR { xy.x = xy.y = HUGE_VAL; return(xy); }
#define I_ERROR { lp.lam = lp.phi = HUGE_VAL; return(lp); }
#define FORWARD(name) static XY name(LP lp, PJ *P) { XY xy
#define INVERSE(name) static LP name(XY xy, PJ *P) { LP lp
#define FREEUP static void freeup(PJ *P) {
#endif

	/* procedure prototypes */
void emess(int, char *, ...);
double  dmstor(const char *, char **);
void set_rtodms(int, int);
char *rtodms(char *, double, int, int);
double adjlon(double);
double aacos(double), aasin(double), asqrt(double);
int gen_cheby(int, UV (), char *, PJ *, int, char **);
PVALUE *pj_param(char *, char *);
double *pj_enfn(double);
double pj_mlfn(double, double, double, double *);
double pj_inv_mlfn(double, double, double *);
double pj_qsfn(double, double, double);
double pj_tsfn(double, double, double);
double pj_msfn(double, double, double);
double pj_phi2(double, double);
double pj_qsfn_(double, PJ *);
double *pj_authset(double);
double pj_authlat(double, double *);
DERIVS *pj_deriv(LP, double, PJ *);
void pj_factors(LP, PJ *);
XY pj_fwd(LP, PJ *);
LP pj_inv(XY, PJ *);
void pj_pr_list(PJ *);
void pj_free(PJ *);
PJ *pj_init(int, char **);

#endif /* end of basic projections header */
