/* @(#)mapgen.h	AMG v.3.4 */
/* mapgen system defintions */
# define VERSION	"III (1/26/89)"
# define MAGIC		9845383
# define MAXPROJ	128
# define MAX_C		121
# define MAX_T_DEG	10
# define MAX_LIST	20

# define LEFT		0x8
# define RIGHT		0x4
# define BOTTOM		0x2
# define TOP		0x1

# define ID_CON		1.e-8
# define DI_CON		1.e8
# define PI		3.1415926535897932384
# define TWO_PI		6.2831853071795864769
# define HALF_PI	1.5707963267948966192
# define R_TO_DEG	57.295779513082320877
# define DEG_TO_R	.01745329251994329576923
# define IONE		100000000
# define CTS_METER	15.7079632679489661923
# define IPI		314159265
# define ITWO_PI	628318531
# define IHALF_PI	157079633
# define DEG_INT	1745329.2519943295769
# define INT_DEG	.00000057295779513082320876798

struct WINDOW_ {
	long	x_min, x_max,
		y_min, y_max;
};

struct map_def {
	long	magic;			/* check tag id			*/
	double	l_lon, r_lon,		/* geographic range		*/
		b_lat, t_lat;
	double	cm; 			/* central meridian		*/
	double	scale;			/* map scale denominator	*/
	double	cts_cm;			/* plotter counts / cm.		*/
	struct	WINDOW_ B;		/* board window			*/
	struct	WINDOW_	D;		/* data sub window		*/
	struct	WINDOW_ S1;		/* data exclusion window 1 *	*/
	struct	WINDOW_ S2;		/* data exclusion window 2 *	*/
	double	cosr, sinr,		/* rotation constants		*/
		xf_off, yf_off;
	double	x_scale, x_off,		/* transformation to +/- 1.	*/
		y_scale, y_off;
	int	x_deg, y_deg;		/* max. degree of approx axis	*/
	int	nxC, nyC;		/* no. of coefficients		*/
	double	xC[MAX_C], yC[MAX_C];	/* Tcheby. coeffs		*/
	char	proj[MAXPROJ];		/* projection name/params	*/
};
/* (*) RFE */

struct srange {			/* employed by georange		*/
	int	c0,		/* edge flags for point 0	*/
		c1;		/* edge flags for point 1	*/
	int	(*m_line)(),	/* line drafting entry		*/
		(*m_point)();	/* character drafting entry	*/
};
