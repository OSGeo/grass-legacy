/*
** @(#)projects.h	Cartographic System v.1.4
**	Cartographic projections system definitions
*/
# include <math.h>
	int
errno;
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

/* default geographic parameters */
# define A_ELLIPS	6378206.4	/* Clarke 1866	*/
# define ES_ELLIPS	.00676865799729	/* Clarke 1866	*/
# define A_SPHERE	6370997.0	/* sphere	*/

# ifdef PROJ_MOD
	/* functions usefull in projections */
extern double adjlon(), phi1_(), phi4_(), mufn_(), authlat(),
	phi2_(), phi3_(), mlfn_(), msfn_(), qsfn_(), tsfn_();

	/* common projection definitions */
# define E_MAX	4
# define U_MAX	4
# define AE_MAX	3
# define a	proj->A
# define es	proj->ES
# define x0	proj->X0
# define y0	proj->Y0
# define lam0	proj->LAM0
# define x	xy.X
# define y	xy.Y
# define lam	lp.LAM
# define phi	lp.PHI
# define phi0	proj->PHI0
# define phi1	proj->PHI1
# define phi2	proj->PHI2
# define e	proj->E
# define e0	proj->E0
	/* 'comset' control */
# define I_ELLPS	1
# define I_LAM0		2
# define I_XY		4
# define I_ALL		I_ELLPS+I_LAM0+I_XY
	/* common parameter heading for all projections */
# define PHEAD	XY *(*forward)(); LP *(*inverse)(); \
	double ES, A, LAM0, X0, Y0
# define INVERSE(n) static LP *n(xy,proj) XY xy; struct _PROJ *proj; { \
	static LP lp
# define FORWARD(n) static XY *n(lp,proj) LP lp; struct _PROJ *proj; { \
	static XY xy
# define ENTRY(n) n(proj, param) struct _PROJ *proj; double *(*param)();
# define ERROR return (0)

# else
	/* normal entry for projections */
	extern UV
*invrs(),
*forwd();
	extern double
dmstor();
	/* projection list structure */
static struct PLIST {
	char	*name;
	int	(*projl)();
};
# endif
