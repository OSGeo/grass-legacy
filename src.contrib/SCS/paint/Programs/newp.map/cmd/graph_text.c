
static int basex, basey ;
static int curx, cury ;

static 
drawchar (
/*
drawchar (size, sinrot, cosrot, character )
*/
    double size,
    double sinrot,
    double cosrot,
    int character,
    int drawflag
)
{
    char *X, *Y;
    register float ax, ay ;
    int i,n;
    int x, y;
    int (*Do)(), draw(), move() ;


    if (size <= 0) size = 2 ;


    x = basex ;  y = basey ;
    get_font_char (character, &n, &X, &Y);

#ifdef DEBUG
fprintf (stdout,"drawchar(%c) basex=%d, basey=%d\n", character, basex, basey);
#endif DEBUG

    Do = move;
    for (i = 1; i < n; i++)
    {
	if (X[i] == ' ')
	{
	    Do = move;
	    continue;
	}
	ax = size * (10 + X[i] - 'R');
	ay = size * (10 - Y[i] + 'R');
        (*Do)(x + (int) (ax * cosrot - ay * sinrot),
            y - (int) (ax * sinrot + ay * cosrot), drawflag ) ;
		 
	Do = draw;
    }

/* ?this next step does variable spacing? */
    ax = size * (10 + X[i] - 'R');
    ay = 0.0;
    move(x + (int) (ax * cosrot - ay * sinrot),
            y - (int) (ax * sinrot + ay * cosrot)) ;
}

static 
draw (int x, int y, int drawflag)
{
#ifdef DEBUG
fprintf (stdout," draw(%d,%d->%d,%d)\n", curx, cury, x, y);
#endif DEBUG
    text_line (curx, cury, x, y, drawflag ) ;   /* see draw_text.c */
    curx = x ;  cury = y ;
}

static 
move (int x, int y)
{
#ifdef DEBUG
fprintf (stdout," move(%d,%d->%d,%d)\n", curx, cury, x, y);
#endif DEBUG
    curx = x ;  cury = y ;
}

# define RpD ((2 * 3.1415926535897932384626434) / 360.0) /* radians/degree */
# define D2R(d) ((double)(d * RpD))         /* degrees->radians */

double sin(), cos() ;

int 
graph_char (int *x, int *y, double size, double rotation, int achar, int drawflag)
{
	float rot = rotation ;
    float sinrot = sin (D2R (rotation)), cosrot = cos (D2R (rotation)) ;
    curx = basex = *x ;
    cury = basey = *y ;
    drawchar (size, sinrot, cosrot, achar , drawflag) ;
    *x = curx ;
    *y = cury ;
}

/* this next routine is not used by Pmap */
int 
graph_text (int *x, int *y, double size, double rotation, register char *string, int drawflag)
{
    float sinrot = sin (D2R (rotation)), cosrot = cos (D2R (rotation)) ;
    curx = basex = *x ;
    cury = basey = *y ;
    do {
        drawchar (size, sinrot, cosrot, *string++, drawflag) ;
        basex = curx ; basey = cury ;
    } while(*string != 00) ;
    *x = curx ;
    *y = cury ;
}
