# define STOP  -1
# define XBIAS  0
# define YBIAS -6

static short int *CharIndex[128] ;
int basex, basey ;
int curx, cury ;

static
drawchar (size, sinrot, cosrot, character)
     float size ; register float sinrot, cosrot ; char character ;
{
    register short int *base ;
    register float ax, ay ;
    int x, y, (*Do)(), draw(), move() ;
    static int inited = 0 ;

    if (! inited)
    {
        initialize () ;
        inited = 1;
    }

    if (size <= 0) size = 2 ;

    base  = CharIndex[character & 0x7F] ;
    x = basex ;  y = basey ;

    while (*base != STOP)
    {
        Do = (*base & 0x8000 ? draw : move) ;
        ax = ((int)(size * (((*base   >> 8) & 0x3F) + XBIAS))) >> 1 ;
        ay = ((int)(size * (( *base++       & 0x3F) + YBIAS))) >> 1 ;
        (*Do)(x + (int) (ax * cosrot - ay * sinrot),
            y - (int) (ax * sinrot + ay * cosrot)) ;
    }
}

static draw (x, y)
{
    graph_line (curx, cury, x, y) ;   /* see graph_line.c */
    curx = x ;  cury = y ;
}

static move (x, y)
{
    curx = x ;  cury = y ;
}

static initialize ()
{
    extern short int Chars[] ;
    register short int *D = Chars ;
    register short int *at ;

    while (*D != STOP)
    /*  { CharIndex[*D] = ++D ; while (*D++ != STOP) ; } */
        { at = D ; CharIndex[*at] = ++D ; while (*D++ != STOP) ; }
}

# define RpD ((2 * 3.1415926535897932384626434) / 360.) /* radians/degree */
# define D2R(d) ((double)(d * RpD))         /* degrees->radians */

double sin(), cos() ;

graph_text (x, y, size, rotation, string)
     int *x, *y ;
     float size ; float rotation ; register char *string ;
{
    float sinrot = sin (D2R (rotation)), cosrot = cos (D2R (rotation)) ;
    curx = basex = *x ;
    cury = basey = *y ;
    do {
        drawchar (size, sinrot, cosrot, *string++) ;
        basex = curx ; basey = cury ;
    } while(*string != 00) ;
    *x = curx ;
    *y = cury ;
}

graph_char (x, y, size, rotation, achar)
     int *x, *y ;
     float size ; float rotation ; register char achar ;
{
    float sinrot = sin (D2R (rotation)), cosrot = cos (D2R (rotation)) ;
    curx = basex = *x ;
    cury = basey = *y ;
    drawchar (size, sinrot, cosrot, achar) ;
    *x = curx ;
    *y = cury ;
}
