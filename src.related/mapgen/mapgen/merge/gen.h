#ifndef lint
static char *gen_hSCCSID = "@(#)gen.h	OEMG v.1.2";
#endif
/* mapgen system defintions */
# define VERSION	"IV (5/18/89)"
# define MAGIC		9845383
# define MAXPROJ	128
# define MAX_C		121
# define MAX_T_DEG	10
# define MAX_LIST	20

# define LEFT		0x8
# define RIGHT		0x4
# define BOTTOM		0x2
# define TOP		0x1
# define F(a,b,c,d,e) (long)(((double)(a-b)*(c-d))/(e-d)+.5)

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
# define PEQ(p1,p2) (p1.x == p2.x && p1.y == p2.y)
/* MAPGEN definition file structure */
struct WINDOW_ {
	long	x_min, x_max,
		y_min, y_max;
};
struct MAPDEF {
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
	long
icm,		/* central meridian */
icp;		/* central parallel */
/* PLOTGEN definition structures */
struct AXIS {
	float	dmin, dmax;	/* world data range		*/
	float	wmin, wmax;	/* data cm. range.		*/
	float	cmboard;	/* board (cm.)			*/
	float	scale, offset;	/* scaling values		*/
	long	board, min, max; /* count ranges		*/
	int	log;		/* log10 conversion		*/
				/* following for 'grid'		*/
	float	del;		/* primary interval		*/
	int	sub;		/* number of subdivisions	*/
};
struct PLTDEF {
	struct AXIS x, y;	/* axis data			*/
	float	cts;		/* counts per cm. (dest plotter)*/
	char	*overlay;	/* ptr to overlay file name	*/
	int	inter;		/* interactive output flag	*/
	int	defer;		/* defer initialization		*/
};
#define PLOTGEN 0
#define MAPGEN  1
	int
data_type;
	long
x_board, y_board;
	int
cts_cm;
/* declarations for expanded 'emess' usage */
	extern int
File_line;
	extern char
*Prog_name,
*File_name;
/* clipping stuff */
#define LINE(a,b,c) c.A=a.y-b.y; c.B=b.x-a.x; c.C=a.x*b.y-a.y*b.x
typedef struct {long x,y;} IXY;
typedef struct {long A,B,C;} GAM;
struct LIMITS {
	IXY	min, max;
	struct LIMITS *next;
	void	(*clip)();
	int	count;
	struct GAMMA { IXY p; GAM g; } L[1];
} base_lim, geog_lim;
	IXY
pl; /* last vector position */
/* maximum sides to  exclusion polynomial */
# define MAX_POLY 100
	void
(*draw)();
	struct MAPDEF
m_def;
	struct PLTDEF
p_def;
	int
oldline,
outside();
	void
mgproj(), pgdraw(), mgdraw(), makewindow(), xwindow(), clip(),
polyclip(), invclip();
	char
*setfield();
