#include "graphics.h"
# define STOP  -1
# define XBIAS  0
# define YBIAS  -6

static short int *CharIndex[128] ;
static int am_inside = 0 ;
static int dont_draw = 0 ;
static int t, b, l, r ;
double basex, basey ;
double curx, cury ;

drawchar (sinrot, cosrot, character)
	register double sinrot, cosrot ; char character ;
{
	register short int *base ;
	register int ax, ay ;
	double x, y ;
	int (*Do)(), text_draw(), text_move() ;
	static int inited = 0 ;

	if (! inited++) initialize () ;

	base  = CharIndex[character & 0x7F] ;
	x = basex ;  y = basey ;

	while (*base != STOP)
	{
		ax = (int)(_text_size_x * (double)(((*base >> 8) & 0x3F) + XBIAS)) >> 1 ;
		ay = (int)(_text_size_y * (double)(( *base       & 0x3F) + YBIAS)) / 2 ;
		if(dont_draw)
		{
			remember(x + (ax * cosrot - ay * sinrot),
				y - (ax * sinrot + ay * cosrot)) ;
		}
		else
		{
			Do = (*base & 0x8000 ? text_draw : text_move) ;
			(*Do)(x + (ax * cosrot - ay * sinrot),
				y - (ax * sinrot + ay * cosrot)) ;
		}
		base++ ;
	}
}

static
remember(x, y)
	double x, y ;
{
	if ((int)x > r) r = (int)x ;
	if ((int)x < l) l = (int)x ;
	if ((int)y > b) b = (int)y ;
	if ((int)y < t) t = (int)y ;
	curx = x ;  cury = y ;
}

static
text_draw (x, y)
	double x, y ;
{
	double X1, Y1 ;
	double X2, Y2 ;
	int mod ;

	X1 = x ;
	Y1 = y ;
	X2 = curx ;
	Y2 = cury ;

	mod = window_clip(&X1, &Y1, &X2, &Y2) ;

	if (am_inside)
	{
		text_line((int)X1, (int)Y1) ;
		if (mod)
			am_inside = 0 ;
	}
	else
	{
		if (!mod)
		{
			Move_abs((int)X2, (int)Y2) ;
			text_line((int)X1, (int)Y1) ;
			am_inside = 1 ;
		}
	}

	curx = x ;  cury = y ;
}

static
text_move (x, y)
	double x, y ;
{
	double X1, Y1 ;
	double X2, Y2 ;
	int mod ;

	X1 = x ;
	Y1 = y ;
	X2 = curx ;
	Y2 = cury ;

	mod = window_clip(&X1, &Y1, &X2, &Y2) ;
	
	if(am_inside)
		Move_abs((int)X1, (int)Y1) ;

	if(mod)
		am_inside = 0 ;

	curx = x ;  cury = y ;
}

static initialize ()
{
	extern short int Chars[] ;
	register short int *D = Chars ;
	register short int *at ;

while (*D != STOP)
	{ at = D ; CharIndex[*at] = ++D ; while (*D++ != STOP) ; }
/*  { CharIndex[*D] = ++D ; while (*D++ != STOP) ; } */
}

# define RpD ((2 * 3.141593) / 360.)	/* radians/degree */
# define D2R(d) (d * RpD)				/* degrees->radians */

double sin(), cos() ;

soft_text_ext (x, y, string)
	int x, y ;
	register char *string ;
{
	t = 999999 ;
	b = 0 ;
	l = 999999 ;
	r = 0 ;
	dont_draw = 1 ;
	soft_text (x, y, string) ;
	dont_draw = 0 ;
}

get_text_ext (top, bot, left, rite)
	int *top, *bot, *left, *rite ;
{
	*top = t ;
	*bot = b ;
	*left = l ;
	*rite = r ;
}

soft_text (x, y, string)
	int x, y ;
	register char *string ;
{
	double sinrot = sin (D2R (_text_rotation)) ;
	double cosrot = cos (D2R (_text_rotation)) ;

	am_inside = 0 ;
	curx = basex = (double)x ;
	cury = basey = (double)y ;
	drawchar (sinrot, cosrot, 00) ;
	basex = curx ; basey = cury ;

	while(*string != 00)
	{
		drawchar (sinrot, cosrot, *string++) ;
		basex = curx ; basey = cury ;
	}
}

onechar (x, y, achar)
	int x, y ;
	register char achar ;
{
	double sinrot = sin (D2R (_text_rotation)) ;
	double cosrot = cos (D2R (_text_rotation)) ;
	am_inside = 0 ;
	curx = basex = (double)x ;
	cury = basey = (double)y ;
	drawchar (sinrot, cosrot, achar) ;
}
