/*  %W%  %G%  */

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "gis.h"
#include "driver.h"

Display *dpy;
Window win;
GC  gc;
XGCValues gc_values;
unsigned long gc_valuemask;
int scrn;
XWindowAttributes winattr;

#include <stdio.h>
#define NL  012
#define TAB 011
#define BACK    0134
#define MTEXT   1024

#define TOP 0
#define CENT    1
#define BOT 2
#define LEFT    0
#define RITE    2
#define YES 1
#define NO  0
static double east;
static double north;
static int xoffset;
static int yoffset;
static int xref;
static int yref;
unsigned long color;
static double size;
static int width;
unsigned long background;
unsigned long border;
static int opaque;
static char text[MTEXT];

extern struct Cell_head window;

initialize_options()
{
    east = 0.0;
    north = 0.0;
    xoffset = 0;
    yoffset = 0;
    xref = CENT;
    yref = CENT;
    color = XD_make_colr(dpy,
        win, scrn,
        winattr.colormap, "black");
    size = 1000.;
    width = 1;
    background = XD_make_colr(dpy,
        win, scrn,
        winattr.colormap, "white");
    border = XD_make_colr(dpy,
        win, scrn,
        winattr.colormap, "black");
    opaque = YES;
}



do_labels(infile)
FILE *infile;
{
    char *fgets();
    char buff[128];
    unsigned int window_width, window_height, border_width, depth;
    int x, y;
    Window root_return;


    /* Set the display to be the default display */
    if ((dpy = XOpenDisplay("")) == NULL) {
        printf(" can't open display\n");
        return (-1);
    }
    scrn = DefaultScreen(dpy);

    win = XD_get_cur_window(dpy,
        scrn);

    XGetWindowAttributes(dpy, win,
        &winattr);


    gc_values.line_width = 1;

    gc = XCreateGC(dpy, win,
        GCLineWidth, &gc_values);

    XGetGeometry(dpy, win,
        &root_return, &x, &y, &window_width,
        &window_height, &border_width, &depth);


    if (XD_do_conversions(&window,
            window_width, window_height))
        G_fatal_error("Error in calculating conversions");

    initialize_options();

    init_font("romant");

    for (;;) {
        if (NULL == fgets(text, MTEXT, infile))
            return;

        if (!strncmp(text, "eas", 3))
            sscanf(text, "%*s %lf", &east);
        else if (!strncmp(text, "nor", 3))
            sscanf(text, "%*s %lf", &north);
        else if (!strncmp(text, "xof", 3))
            sscanf(text, "%*s %d", &xoffset);
        else if (!strncmp(text, "yof", 3))
            sscanf(text, "%*s %d", &yoffset);
        else if (!strncmp(text, "col", 3)) {
            sscanf(text, "%*s %s", buff);
            color = XD_make_colr(dpy,
                win, scrn,
                winattr.colormap, buff);
        } else if (!strncmp(text, "siz", 3))
            sscanf(text, "%*s %lf", &size);
        else if (!strncmp(text, "wid", 3))
            sscanf(text, "%*s %d", &width);
        else if (!strncmp(text, "bac", 3)) {
            sscanf(text, "%*s %s", buff);
            background = XD_make_colr(dpy,
                win, scrn,
                winattr.colormap, buff);
        } else if (!strncmp(text, "bor", 3)) {
            sscanf(text, "%*s %s", buff);
            border = XD_make_colr(dpy,
                win, scrn,
                winattr.colormap, buff);
        } else if (!strncmp(text, "opa", 3)) {
            sscanf(text, "%*s %s", buff);
            if (!strncmp(buff, "YES", 3))
                opaque = YES;
            else
                opaque = NO;
        } else if (!strncmp(text, "ref", 3)) {
            if (sscanf(text, "%*s %s", buff) < 1 || scan_ref(buff) == 0) {
                xref = CENT;
                yref = CENT;
            }
        } else if (!strncmp(text, "tex", 3)) {
            show_it();
            XFlush(dpy);
        }
    }

}

show_it()
{
    int n_lines;
    int n_chars;
    char line[256];
    char *lptr, *tptr;
    double line_size;
    int text_size;
    int X, Y;
    int T, B, L, R;
    int scrT, scrB, scrL, scrR;
    int t, b, l, r;
    int Xoffset;
    int Yoffset;
    double XD_u_to_d_row();
    double XD_u_to_d_col();
    double XD_get_d_west();
    double XD_get_d_east();
    double XD_get_d_north();
    double XD_get_d_south();

    X = (int) XD_u_to_d_col(east);

	/* Set text size */
    text_size = XD_u_to_d_row((double) 0) - XD_u_to_d_row(size);
    Text_size(text_size, text_size);
    line_size = size * 1.2;

/* Find extent of all text (assume ref point is upper left) */
    T = 999999;
    B = 0;
    L = 999999;
    R = 0;

/* Scan to beginning of text string */
    for (tptr = text; *tptr != ':'; tptr++);
    tptr++;

    n_lines = 0;
    for (;;) {
        n_chars = 0;
        for (lptr = line; *tptr && *tptr != NL; *lptr++ = *tptr++) {
            if ((*tptr == BACK) && (*(tptr + 1) == 'n'))
                break;
            n_chars++;
        }
        n_lines++;

        if (n_chars == 0)
            break;

        *lptr = NULL;

        Y = (int) (XD_u_to_d_row(north - size - (n_lines - 1) * line_size));
        Move_abs(X, Y);
        Get_text_box(line, &t, &b, &l, &r);
        if (t < T)
            T = t;
        if (b > B)
            B = b;
        if (l < L)
            L = l;
        if (r > R)
            R = r;

        if ((*tptr == NULL) || (*tptr == NL))
            break;
        tptr++;
        tptr++;
    }

    /* Expand border 1/2 of text size */
    T = T - text_size / 2;
    B = B + text_size / 2;
    L = L - text_size / 2;
    R = R + text_size / 2;

    Xoffset = xoffset;
    Yoffset = yoffset;

    if (xref == CENT)
        Xoffset -= (R - L) / 2;
    if (xref == RITE)
        Xoffset -= R - L;
    if (yref == CENT)
        Yoffset -= (B - T) / 2;
    if (yref == BOT)
        Yoffset -= B - T;

/* Draw box */
    scrL = L + Xoffset;
    scrR = R + Xoffset;
    scrT = T + Yoffset;
    scrB = B + Yoffset;

     /* If the window is outside of current map window, ignore */ ;
    if (scrR < (int) XD_get_d_west())
        return;
    if (scrL > (int) XD_get_d_east())
        return;
    if (scrB < (int) XD_get_d_north())
        return;
    if (scrT > (int) XD_get_d_south())
        return;

    /* Clip parts of label to inside map window */
    if (scrL < (int) XD_get_d_west())
        scrL = (int) XD_get_d_west();
    if (scrR > (int) XD_get_d_east())
        scrR = (int) XD_get_d_east();
    if (scrT < (int) XD_get_d_north())
        scrT = (int) XD_get_d_north();
    if (scrB > (int) XD_get_d_south())
        scrB = (int) XD_get_d_south();
/*

    pts[0].x = (short) scrL ;
    pts[1].x = (short) scrL ;
    pts[2].x = (short) scrR ;
    pts[3].x = (short) scrR ;
    pts[4].x = (short) scrL ;
    pts[0].y = (short) scrB ;
    pts[1].y = (short) scrT ;
    pts[2].y = (short) scrT ;
    pts[3].y = (short) scrB ;
    pts[4].y = (short) scrB ;
*/

    XSetForeground(dpy, gc, background);
/*
    XFillPolygon(dpy, win, gc,
        (XPoint *) &pts[0], 5, Complex,
        CoordModeOrigin);
*/
    XFillRectangle(dpy, win, gc,
        scrL + 1, scrT + 1, scrR - scrL - 1, scrB - scrT - 1);

/* Draw border
    pts[0].x -= delta;
    pts[0].y += delta;
    pts[4].x -= delta;
        pts[4].y += delta;
    pts[1].x -= delta;
    pts[3].y += delta;
*/

    XSetForeground(dpy, gc, border);
/*
    XDrawLines(dpy, win, gc,
        (XPoint *) &pts[0], 5, CoordModeOrigin);
*/
    XDrawRectangle(dpy, win, gc,
        scrL, scrT, scrR - scrL, scrB - scrT);

    for (tptr = text; *tptr != ':'; tptr++);
    tptr++;

/* Draw text */
    XSetForeground(dpy, gc, color);

    n_lines = 0;
    for (;;) {
        n_chars = 0;
        for (lptr = line; *tptr && *tptr != NL; *lptr++ = *tptr++) {
            if ((*tptr == BACK) && (*(tptr + 1) == 'n'))
                break;
            n_chars++;
        }

        n_lines++;
        if (n_chars == 0)
            break;

        *lptr = NULL;

        Y = (int) (XD_u_to_d_row(north - size - (n_lines - 1) * line_size));

/*
        R_set_window(scrT, scrB, scrL, scrR) ;
*/

        Move_abs(X + Xoffset, Y + Yoffset);
        Text(line);

        if ((*tptr == NULL) || (*tptr == NL))
            break;
        tptr++;
        tptr++;
    }
}

static xok, yok;

scan_ref(buf)
char *buf;
{
    char word1[50], word2[50];
    int i;

    xok = yok = 0;

    for (i = 0; buf[i]; i++)
        if (buf[i] >= 'A' && buf[i] <= 'Z')
            buf[i] += 'a' - 'A';
    xref = yref = CENT;
    switch (sscanf(buf, "%s%s", word1, word2)) {
    case 2:
        if (!(xmatch(word2) || ymatch(word2)))
            return 0;
    case 1:
        if (xmatch(word1) || ymatch(word1))
            return 1;
    default:
        return 0;
    }
}

static xmatch(word)
char *word;
{
    if (strcmp(word, "center") == 0)
        return 1;
    if (strcmp(word, "middle") == 0)
        return 1;
    if (xok)
        return 0;

    if (strcmp(word, "left") == 0)
        xref = LEFT;
    else if (strcmp(word, "right") == 0)
        xref = RITE;
    else
        return 0;
    xok = 1;
    return 1;
}

static ymatch(word)
char *word;
{
    if (strcmp(word, "center") == 0)
        return 1;
    if (strcmp(word, "middle") == 0)
        return 1;
    if (yok)
        return 0;

    if (strcmp(word, "upper") == 0)
        yref = TOP;
    else if (strcmp(word, "top") == 0)
        yref = TOP;
    else if (strcmp(word, "lower") == 0)
        yref = BOT;
    else if (strcmp(word, "bottom") == 0)
        yref = BOT;
    else
        return 0;
    yok = 1;
    return 1;
}
