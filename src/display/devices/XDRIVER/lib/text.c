#define STOP  -1

static int am_inside = 0;
static int dont_draw = 0;
static int t, b, l, r;
double basex, basey;
double curx, cury;
static text_draw(), text_move();
double sin(), cos();

drawchar(_text_size_x, _text_size_y, sinrot, cosrot, character)
double _text_size_x, _text_size_y;
register double sinrot, cosrot;
char character;
{
    unsigned char *X, *Y;
    register int ax, ay;
    int n_vects, i;
    int (*Do) ();
    int ix, iy;
    double x, y;

    x = basex;
    y = basey;
    get_char_vects(character, &n_vects, &X, &Y);

    Do = text_move;
    for (i = 1; i < n_vects; i++) {
        if (X[i] == ' ') {
            Do = text_move;
            continue;
        }
        ix = 10 + X[i] - 'R';
        iy = 10 - Y[i] + 'R';
        ax = (int) (_text_size_x * (double) ix);
        ay = (int) (_text_size_y * (double) iy);
        if (dont_draw) {
            remember(x + (ax * cosrot - ay * sinrot),
                    y - (ax * sinrot + ay * cosrot));
        } else {
            (*Do) (x + (ax * cosrot - ay * sinrot),
                    y - (ax * sinrot + ay * cosrot));
            Do = text_draw;
        }
    }
    /* This seems to do variable spacing ix = 10 + X[i] - 'R' ; */
    ix = 20;
    iy = 0;
    ax = (int) (_text_size_x * (double) ix);
    ay = (int) (_text_size_y * (double) iy);
    if (!dont_draw)
        text_move(basex + (ax * cosrot - ay * sinrot),
                basey - (ax * sinrot + ay * cosrot));
    else
        remember(basex + (ax * cosrot - ay * sinrot),
                basey - (ax * sinrot + ay * cosrot));
}


static remember(x, y)
double x, y;
{
    if ((int) x > r)
        r = (int) x;
    if ((int) x < l)
        l = (int) x;
    if ((int) y > b)
        b = (int) y;
    if ((int) y < t)
        t = (int) y;
    curx = x;
    cury = y;
}

static text_draw(x, y)
double x, y;
{
    double X1, Y1;
    double X2, Y2;
    int mod;

    X1 = x;
    Y1 = y;
    X2 = curx;
    Y2 = cury;

    mod = window_clip(&X1, &Y1, &X2, &Y2);

    if (am_inside) {
        Cont_abs((int) X1, (int) Y1);
        if (mod)
            am_inside = 0;
    } else {
        if (!mod) {
            Move_abs((int) X2, (int) Y2);
            Cont_abs((int) X1, (int) Y1);
            am_inside = 1;
        }
    }
    curx = x;
    cury = y;
}


static text_move(x, y)
double x, y;
{
    double X1, Y1;
    double X2, Y2;
    int mod;

    X1 = x;
    Y1 = y;
    X2 = curx;
    Y2 = cury;

    mod = window_clip(&X1, &Y1, &X2, &Y2);
    if (am_inside)
        Move_abs((int) X1, (int) Y1);
    if (mod)
        am_inside = 0;
    curx = x;
    cury = y;
}

soft_text_ext(x, y, _text_size_x, _text_size_y, _text_rotation, string)
int x, y;
double _text_size_x, _text_size_y, _text_rotation;
char *string;
{
    t = 999999;
    b = 0;
    l = 999999;
    r = 0;
    dont_draw = 1;
    soft_text(x, y, _text_size_x, _text_size_y, _text_rotation, string);
    dont_draw = 0;
}

get_text_ext(top, bot, left, rite)
int *top, *bot, *left, *rite;
{
    *top = t;
    *bot = b;
    *left = l;
    *rite = r;
}

#define RpD ((2 * 3.141593) / 360.)     /* radians/degree */
#define D2R(d) (double)(d * RpD)/* degrees->radians */

/* ARGSUSED */
soft_text(x, y, _text_size_x, _text_size_y, _text_rotation, string)
int x, y;
double _text_size_x, _text_size_y, _text_rotation;
char *string;
{
    double sinrot = 0.0;
    double cosrot = 1.0;

    am_inside = 0;
    curx = basex = (double) x;
    cury = basey = (double) y;
    while (*string != 00) {
        drawchar(_text_size_x, _text_size_y, sinrot, cosrot, *string++);
        basex = curx;
        basey = cury;
    }
}

onechar(x, y, _text_size_x, _text_size_y, _text_rotation, achar)
int x, y;
double _text_size_x, _text_size_y, _text_rotation;
register char achar;
{
    double sinrot = 0.0;
    double cosrot = 1.0;

    am_inside = 0;
    curx = basex = (double) x;
    cury = basey = (double) y;
    drawchar(_text_size_x, _text_size_y, sinrot, cosrot, achar);
}
