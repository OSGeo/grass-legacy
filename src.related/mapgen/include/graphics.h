#ifndef lint
static char *SCCSgraphics_h = "@(#)graphics.h	USGS v.4.1";
#endif

# define _LONG   0x60
# define _BYTE   0x20
# define _LBMASK 0x1f
# define _STR    0xa0
# define _NOARG  0x80
# define _COORD  0xc0

/* _COORDinate modifiers */
# define _PENUP 0x10
# define _REL	0x20

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
/* special internal commands */
# define P_REQ	31+_BYTE

/* no argument set */
# define DISABLE 0+_NOARG
# define DASH	1+_NOARG
# define SOLID	2+_NOARG
# define JLEFT	3+_NOARG
# define JRIGHT	4+_NOARG
# define CENTER 5+_NOARG
# define ERASE	6+_NOARG
# define DELPEN 13+_NOARG
# define DELINK 14+_NOARG
# define CBASE	18+_NOARG
# define FPLOT	20+_NOARG
# define FPLOTN	21+_NOARG
# define BEZIER	22+_NOARG
# define BEZIERN 23+_NOARG
/* special internal commands */
# define P_ACK	31+_NOARG

/* string set */
# define SPECIAL 0+_STR
# define TEXT	7+_STR
# define INCL	8+_STR
# define SPEN	9+_STR
# define SFONT	10+_STR
# define SFONTA	11+_STR
# define LINKXY 12+_STR
# define NEWPEN	15+_STR
# define SFONTS	16+_STR
# define RESCALE 17+_STR
# define FSYMS	19+_STR

/* request codes (all undefined codes are considered no_ops) */
# define ERROR	1
# define P_SIZE 2
# define CURSOR 3
# define FSIZE  4
# define FSSIZE 5

/* error codes from plotter */
# define E_INCL		1	/* can't open include */
# define E_INCLOV	2	/* include level overflow */
# define E_OPC		3	/* invalid operation */
# define E_PALLOC	4	/* unable to allocate memory */
# define E_NOPEN	5	/* no pen defined */
# define E_NOTMPL	6	/* new pen template not found */
# define E_EQNAME	7	/* new pen name exists */
# define E_FNOFIL	8	/* no font file */
# define E_FREAD	9	/* read failure on font input */
# define E_FALLOC	10	/* failure to allocate font memory */
# define E_FOPEN	11	/* link or file failure on font fetch */
# define E_WALLOC	12	/* window allocation failure */
# define E_LINK		13	/* failure to find link pen */
# define E_BADRQ	14	/* invalid request code */
# define E_MASK		15	/* dash line mask in error */
# define E_DSIZE	16	/* dash size <= 0 */
# define E_SIZE		17	/* character size <= 0 */
# define E_SSIZE	18	/* symbol size <= 0 */
# define E_RESCL	19	/* scaling while pen(s) active */
# define E_NOCURSOR	20	/* cursor req'st to dev. with no cursor */
# define E_BADDEV	21	/* invalid device selected */

/* definitions for users only */
#ifndef PLOTTER

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
typedef struct _answr {	/* data returned by request command */
	int cmd;	/* last data type */
	long x, y, code;	/* double - byte | long */
	char str[MAX_USTR+1];	/* string */
} ANSWR;
extern ANSWR *plotreq();
typedef struct {
	long x, y;	/* base coordinates */
	struct _answr answr;	/* result from 'plotter' query */
} BASE;

#endif

/* for external drivers only */
#ifdef EXTDEV

/* tag definitions for external driver(s)
**	The intermediate meta-graphic stream is determined as follows:
**
**	1. Start of plot 0xf0
**	2. End of plot	 0xff
**	3. New pen	 0xf1 0xnn(hi) 0xnn(lo) (pen #)
**	4. Move (pen up) 0xf2 0xnn(hi x) 0xnn(lo x) 0xnn(hi y) 0xnn(lo y)
**	5. Draw		 0xf3 0xnn(hi x) 0xnn(lo x) 0xnn(hi y) 0xnn(lo y)
**	6. Special Str   0xf4 null terminated string
**	7. New pen(long) 0xf5 0xnn(msb) 0xnn() 0xnn(lsb) (24 bit pen)
**
**	Note: coordinates are considered unsigned, 16 bit.
*/
#define _BOP 0xf0
#define _EOP 0xff
#define _PEN 0xf1
#define _PENL 0xf5
#define _MOVE 0xf2
#define _DRAW 0xf3
#define _SPCL 0xf4

#endif
