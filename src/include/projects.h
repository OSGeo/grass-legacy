/* General projections header file */
#ifndef __PROJECTS_H
#define __PROJECTS_H

    /* standard inclusions */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>



#ifndef lint
static char PROJECTS_H_ID[] = "@(#)$Id: projects.h,v 4.5 1992/07/14 01:42:26 gie Exp $";
#endif

	/* some useful constants */
#define HALFPI		1.5707963267948966
#define FORTPI		0.78539816339744833
#define PI		3.14159265358979323846
#define TWOPI		6.2831853071795864769
#define RAD_TO_DEG	57.29577951308232
#define DEG_TO_RAD	.0174532925199432958

#ifndef HUGE_VAL
#define HUGE_VAL	HUGE
#endif

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

#ifndef PROTO

#ifdef __STDC__
# define	PROTO(s) s
#else
# define PROTO(s) ()
#endif

#endif	/* PROTO */



	/* base projection data structure */
typedef struct PJconsts {
	XY  (*fwd) PROTO((LP, struct PJconsts *));
	LP  (*inv) PROTO((XY, struct PJconsts *));
	paralist *params;   /* parameter list */
	void (*pfree) PROTO((struct PJconsts *));
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
#define E_ERROR { freeup(P); return(0); }
#define F_ERROR { xy.x = xy.y = HUGE_VAL; return(xy); }
#define I_ERROR { lp.lam = lp.phi = HUGE_VAL; return(lp); }

#ifdef __STDC__
# define ENTRY(name) PJ *name (PJ *P)
# define FORWARD(name) static XY name(LP lp, PJ *P) 
# define INVERSE(name) static LP name(XY xy, PJ *P)
# define FREEUP static void freeup(PJ *P) 
#else
# define ENTRY(name) PJ *name (P) PJ *P;
# define FORWARD(name) static XY name(lp, P) LP lp ; PJ *P;
# define INVERSE(name) static LP name(xy, P) XY xy ; PJ *P;
# define FREEUP static void freeup(P) PJ *P;
#endif

#endif



/* Error message processing header file */
#ifndef __EMESS_H
#define __EMESS_H

#ifndef lint
static char EMESS_H_ID[] = "@(#)$Id: emess.h,v 4.1 1992/07/02 12:54:05 gie Exp $";
#endif

struct EMESS {
    char *File_name,	/* input file name */
	 *Prog_name;	/* name of program */
    int	 File_line;	/* approximate line read where error occured */
};

#ifdef _emess_routine	/* use type */
/* for emess procedure */
struct EMESS emess_dat = { (char *)0, (char *)0, 0 };

#if defined(sun) || (defined(mips) && !defined(sgi)) /* Archaic SunOs 4.1.1, etc. */
extern char *sys_errlist[];
#define strerror(n) (sys_errlist[n])
#endif

#else	/* for for calling procedures */

extern struct EMESS emess_dat;

void emess PROTO((int, char *, ...));

#endif /* use type */

#endif /* end __EMESS_H */

/* Function Prototypes */
double aasin      PROTO((double v ));
double aacos      PROTO((double v ));
double asqrt      PROTO((double v ));
double adjlon     PROTO((double lon ));
int    bchgen     PROTO((UV a, UV b, int nu, int nv, UV **f, UV (*func )(UV )));
double dmstor     PROTO((const char *is , char **rs ));
void   emess      PROTO((int code , char *fmt , ...));
int    gen_cheby  PROTO((int inverse, UV proj (), char *s, PJ *P, int iargc, char **iargv));
double *pj_authset PROTO((double es ));
double pj_authlat PROTO((double beta , double *APA ));
DERIVS *pj_deriv  PROTO((LP lp , double h , PJ *P ));
void   pj_factors PROTO((LP lp , PJ *P ));
XY     pj_fwd     PROTO((LP lp , PJ *P ));
PVALUE *pj_param  PROTO((char *opt , char *def ));
PJ     *pj_init   PROTO((int argc , char **argv ));
void   pj_free    PROTO((PJ *P ));
void   pj_pr_list PROTO((PJ *P ));
LP     pj_inv     PROTO((XY xy , PJ *P ));
double *pj_enfn   PROTO((double es ));
double pj_mlfn    PROTO((double phi , double sphi , double cphi , double *en ));
double pj_inv_mlfn PROTO((double arg , double es , double *en ));
double pj_msfn    PROTO((double sinphi , double cosphi , double es ));
LP     pj_ninvrs  PROTO((XY xy , LP lp , PJ *P ));
double pj_phi2    PROTO((double ts , double e ));
double pj_qsfn    PROTO((double sinphi , double e , double one_es ));
double pj_tsfn    PROTO((double phi , double sinphi , double e ));
void   set_rtodms PROTO((int fract , int con_w ));
char   *rtodms    PROTO((char *s , double r , int pos , int neg ));


#endif /* end of basic projections header */



struct pj_info {
      PJ     *pj;
      double meters;
      int    zone;
      char   proj[100];
};

