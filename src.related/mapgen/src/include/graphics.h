/* @(#)graphics.h	AMG v.3.4 */

# define _LONG   0x60
# define _BYTE   0x20
# define _LBMASK 0x1f
# define _STR    0xa0
# define _NOARG  0x80
# define _COORD  0xc0

/* byte/long argument set */

# define WXL	0+_LONG
# define WXH	1+_LONG
# define WYL	2+_LONG
# define WYH	3+_LONG

# define SIZE	4+_LONG
# define ANG	5+_LONG
# define _ANGCVT 10000.0
# define XOFF	6+_LONG
# define YOFF	7+_LONG

# define MPEN	8+_LONG

# define DMASK	9+_LONG
# define DSIZE	10+_LONG

# define SYM	11+_BYTE

# define SSIZE	12+_LONG
# define SANG	13+_LONG
# define LEAD	14+_LONG
# define BASEX	15+_LONG
# define BASEY	16+_LONG
# define F_SIZE	17+_LONG
# define F_DIST	18+_LONG

/* string/no argument set */

# define DISABLE 0+_NOARG
# define DASH	1+_NOARG
# define SOLID	2+_NOARG

# define JLEFT	3+_NOARG
# define JRIGHT	4+_NOARG
# define CENTER 5+_NOARG

# define ERASE	6+_NOARG

# define TEXT	7+_STR
# define INCL	8+_STR
# define SPEN	9+_STR
# define SFONT	10+_STR
# define SFONTA	11+_STR
# define LINKXY 12+_STR
# define DELPEN 13+_NOARG
# define DELINK 14+_NOARG
# define NEWPEN	15+_STR
# define SFONTS	16+_STR
# define RESCALE 17+_STR
# define CBASE	18+_NOARG
# define FSYMS	19+_STR
# define FPLOT	20+_NOARG
# define FPLOTN	21+_NOARG
# define BEZIER	22+_NOARG
# define BEZIERN 23+_NOARG

/* request codes (all undefined codes are considered no_ops) */
# define ERROR	1
# define P_SIZE 2
# define CURSOR 3
# define FSIZE  4
# define FSSIZE 5

/* special internal commands */
# define P_REQ	31+_BYTE
# define P_ACK	31+_NOARG

# define _PENUP 0x10
# define _REL	0x20

#ifndef PLOTTER
/* definitions for users only */

	/* make high resolution form of character size */
#define fcharsz(s) (long)(-16.*(s) - .5)
#define moveto(x,y) pxyxmit(_PENUP,(long)(x),(long)(y))
#define lineto(x,y) pxyxmit(0,(long)(x),(long)(y))
#define relmoveto(x,y) pxyxmit(_PENUP+_REL,(long)(x),(long)(y))
#define rellineto(x,y) pxyxmit(_REL,(long)(x),(long)(y))
#define erase() plotopt(ERASE)
#define string(x) plotopt(TEXT,(char *)(x))
#define angle(x) plotopt(ANG,(long)((x) * _ANGCVT))

#define MAX_USTR 20
struct _answr {		/* data returned by request command */
	int cmd;	/* last data type */
	long x, y, code;	/* double - byte | long */
	char str[MAX_USTR+1];	/* string */
};
#define ANSWR struct _answr

extern ANSWR *plotreq();

struct _ubase {
	long x, y;	/* base coordinates */
	ANSWR answr;	/* result from 'plotter' query */
};
#define BASE struct _ubase

#endif

#ifdef EXTDEV

/* tag definitions for external driver(s)
**	The intermediate meta-graphic stream is determined as follows:
**
**	1. Start of plot 0xf0
**	2. End of plot	 0xff
**	3. New pen	 0xf1 0xnn(pen #)
**	4. Move (pen up) 0xf2 0xnn(hi x) 0xnn(lo x) 0xnn(hi y) 0xnn(lo y)
**	5. Draw		 0xf3 0xnn(hi x) 0xnn(lo x) 0xnn(hi y) 0xnn(lo y)
**
**	Note: coordinates are considered unsigned, 16 bit.
*/
#define _BOP 0xf0
#define _EOP 0xff
#define _PEN 0xf1
#define _MOVE 0xf2
#define _DRAW 0xf3

#endif
