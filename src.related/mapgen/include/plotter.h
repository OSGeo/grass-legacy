#ifndef lint
static char *SCCSplotter_h = "@(#)plotter.h	USGS v.4.4";
#endif
/* definitions file for device driver only */
# define VERSION "4.4\n"
# define PERR 0x80
# include <stdio.h>
/* device commands: */
# define D_INIT		0
# define D_MOVE		1
# define D_LINE		2
# define D_PEN		3
# define D_ERASE	4
# define D_CURSOR	5
# define D_DISABL	6
# define D_SCALE	7
# define D_DONE		9
# define D_PANIC	10
# define D_STRING	11

# define MAX_DARGS	20
# define MAX_F_SYMS	128
# define MAX_STABLE	512
struct _stable {	/* scaled and rotated character table */
	short l, r, b, t;		/* left and right size */
	short xlo, xhi, ylo, yhi;	/* character range */
	char pen[MAX_STABLE];		/* pen up control */
	short x[MAX_STABLE], y[MAX_STABLE]; /* pen vectors */
};
# define STABLE struct _stable

# define FONTNAME 80
struct _font {		/* font structure */
	struct _font *next;	/* link to next entry */
	float rsize;		/* 21 / vect[0] -- size normalizer */
	char name[FONTNAME];	/* name of font */
	unsigned short size;	/* length of font tables */
	unsigned short dir[128]; /* offsets to char in 'vect' */
	char vect[1];		/* vector table, vect[0] = vertical size */
};
# define FONT struct _font

/* font selection */
# define ACHAR	2
# define PCHAR	1

/* pen coordinates and window */
struct _window {
	long x[3], y[3];
	long xlo, xhi, ylo, yhi;
};
# define WINDOW struct _window

# define MAXPEN 256
	/* the following apply to '.cflags' */
# define C_THETA	0x01	/* rotation flag */
# define C_RIGHT	0x02	/* right just */
# define C_CENTER	0x04	/* center */
# define C_SYM		0x08	/* symbol mode */
# define C_STHETA	0x10	/* symbol rotation */
# define C_AFONT	0x20	/* alternate font */
# define _FLINE		0x01	/* draft lines through fancy symbols */
# define _FLIP		0x02	/* flip orientation of fancy symbol */

# define MAX_PENS 32
struct _pen {
	char name[MAXPEN];	/* logical pen name */
	struct _pen *next;	/* to next pen in list */
	WINDOW local;		/* x-y position struct */
	WINDOW *xy;		/* to x-y struct */
	unsigned long mpen;	/* mechanical pen */
			/* character control section */
	FONT *font, *afont;	/* primary and alt. fonts */
	int cflags;		/* flags */
	float cost, sint;	/* tranformation multipliers */
	float bcost, bsint;	/* tranformation multipliers * 'csize' */
	long csize;		/* size multiplier * 16 */
	long xoff, yoff;	/* offsets */
	long r_xoff, r_yoff;	/* rotated offsets */
	int leading;		/* leading (inter line spacing) */
			/* symbol section */
	FONT *sfont;		/* symbol font */
	float scost, ssint;	/* unscaled rotation */
	float sbcost, sbsint;	/* scaled rotation */
	long ssize;		/* symbol size * 16 */
	int sym;		/* current symbol */
			/* line drafting section */
	int (*line)();		/* line routine */
	int dindex;		/* dashline index to darray */
	short darray[16];	/* size of dash line segments */
	int damax;		/* number of segments in dashed line */
	int dresid;		/* residual from last node */
	int dsize;		/* elemental size */
	int dmask;		/* dash bit mask */
	int pendown;		/* !=0 pen down */
	int bezindx;		/* Bezier array index */
	int bezierm;		/* Bezier mode flag */
			/* feature ticks sub-section */
	int (*dsline)();	/* ticked or solid */
	int f_pline;		/* plot line between symbols */
	int f_nosym;		/* number of tick symbols */
	int f_cycle;		/* feature list cycle */
	char f_syms[MAX_F_SYMS]; /* symbol list */
	long f_size;		/* symbol size */
	int f_dist;		/* inter-symbol dist */
	int f_rdist;		/* residual from last symbol */
};
# define PEN struct _pen

# define X pen->xy->x[pen->bezindx]
# define Y pen->xy->y[pen->bezindx]

# define BIGLONG 0x7fffff
struct _base {
	long	x, y;		/* current base coordinates */
	char	*text;		/* current text */
};
# define BASE struct _base

typedef struct _xy { long x, y; } XY;
typedef struct _xys { long x, y; char *s; } XYS;

# define caseof(x) case x & _LBMASK
struct DEV_LIST {
	char *name;	/* device name referenced by -d option	*/
			/* or TERM environment parameter	*/
	XY *(*dev)();	/* device dependent driver routine */
	int model;	/* Device options for small variations in generic
			   device.  See individual drivers for meaning */
};
	/* following globals referenced by device drivers */
struct {
	double scale;	/* basic scaling parameter */
	int dargc;	/* count of device (-D) args */
	char *dargv[MAX_DARGS+3]; /* list of device (-D) args */
	int reverse;	/* reverse axis */
	int model_no;	/* plotter model */
	int quiet;	/* mute <bell> wait response at end of plot */
} Dglobal
#ifdef MAIN_PROG
= { 1., 1 }; /* remainder default to zero */
#else
;
#endif
FILE *wpopen();
