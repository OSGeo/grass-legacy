#ifndef lint
static char *SCCSprojects_h = "@(#)projects.h	USGS v.3.3";
#endif
/* <<<< Cartographic projections system definitions >>>> */
# include <math.h>
typedef struct { double X, Y; }		XY;
typedef struct { double LAM, PHI; }	LP;
/* generic form */
typedef struct { double u, v; }		UV;
	/* some useful constants */
# define HALFPI		1.5707963267948966
# define FORTPI		0.78539816339744833
# define PI		3.14159265358979323846
# define TWOPI		6.2831853071795864769
# define RAD_TO_DEG	57.29577951308232
# define DEG_TO_RAD	.0174532925199432958
	/* global ellipsoid definition structure */
struct {
	double	A,	/* major axis or radius if es==0 */
		E,	/* eccentricity */
		ES,	/* e ^ 2 */
		RA,	/* 1/A */
		ONE_ES,	/* 1 - e^2 */
		RONE_ES, /* 1/ONE_ES */
		LAM0, PHI0, /* central longitude, latitude */
		X0, Y0;	/* easting and northing */
} ELLIPSE;
/* the following allow for simpler reference in
** math statements (rather than expanded . form)
*/
# define a	ELLIPSE.A
# define e	ELLIPSE.E
# define es	ELLIPSE.ES
# define ra	ELLIPSE.RA
# define one_es	ELLIPSE.ONE_ES
# define rone_es ELLIPSE.RONE_ES
# define x0	ELLIPSE.X0
# define y0	ELLIPSE.Y0
# define lam0	ELLIPSE.LAM0
# define phi0	ELLIPSE.PHI0
	/* functions useful in projections */
double	phi1_(), phi4_(), authlat(), *param(), adjlon(),
	phi2_(), mlfn_(), inv_mlfn_(), msfn_(), qsfn_(), tsfn_();
void	enfn_(), authset();
	/* common projection definitions */
# define x	xy.X
# define y	xy.Y
# define lam	lp.LAM
# define phi	lp.PHI
	/* common parameter heading for all projections */
# define INVERSE(n) static LP n(xy) XY xy; { static LP lp
# define NULL_INVERSE(n) static LP (*n)() = (LP(*)()) 0;
# define FORWARD(n) static XY n(lp) LP lp; { static XY xy
# define NULL_FORWARD(n) static XY (*n)() = (XY(*)()) 0;
# define ENTRY(n) UV(*n(inverse))() int inverse;
# define RETURN(n) return( (UV(*)())(n) )
# define F_ERROR { x = HUGE; return(xy); }
# define I_ERROR { lam = HUGE; return(lp); }
# define E_ERROR RETURN(0)
