#include <stdio.h>
#include "gis.h"
#include <math.h>
#include "flag.h"
#include "cseg.h"

#define NODE		struct _n_o_d_e_
#define SHORT		short
#define INIT_AR		64
#define AR_INCR		64
#define ABS(x)		(((x) < 0) ? -(x) : (x))
#define MIN(x,y)	(((x) < (y)) ? (x) : (y))
#define TST(x)		/* fprintf(stderr,"(x):%d\n",(x)) */
#define TSTSTR(x)	/* fprintf(stderr,"%s\n",(x)) */

NODE
{
	SHORT	r, c;
	double	d;
};

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL SHORT	nrows;
GLOBAL SHORT	ncols;
GLOBAL SHORT	minc;
GLOBAL SHORT	minr;
GLOBAL SHORT	maxc;
GLOBAL SHORT	maxr;
GLOBAL int	array_size;
GLOBAL double	i_val_l_f;
GLOBAL CSEG	con;
GLOBAL FLAG	*seen;
GLOBAL BSEG	bseen;
GLOBAL NODE	*zero;
GLOBAL CELL	on, off;
