/*
**  Written by Terry Baker 5/1993
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
/* routines to handle redrawing screen when an expose event occurs */

#define HUGE_VAL 1000000
#define NUM_ELEMENTS 50
static int display_list[NUM_ELEMENTS]; /* list of element drawing routines */
				       /* called since last redraw */
static int curr = 0;                   /* current index into display list */
static int ClearFirst = 0;   /* was window cleared before display routines ? */

static int line = 0;         /* info about currently highlighted line */
static char type = 0;
static struct line_pnts *points = NULL;
static struct line_pnts *CPoints = NULL;


void
expose(w, data, cbs)
    Widget  w;
    caddr_t data;
    XmDrawingAreaCallbackStruct *cbs;
{
    static int first = 1;
    int sx1, sy1, sx2, sy2;
    double ux1, uy1, ux2, uy2;
    static int minx, miny, maxx, maxy;
    XGCValues values;
    int tmp;

    if (!CPoints)        /* init point struct for highlit lines */
	CPoints = Vect_new_line_struct();
    
    sx1 = cbs->event->xexpose.x;      /* get size info about exposed area */
    sy1 = cbs->event->xexpose.y;
    sx2 = sx1 + cbs->event->xexpose.width;
    sy2 = sy1 + cbs->event->xexpose.height;

    if (first) 
    {
        minx = HUGE_VAL;
        miny = HUGE_VAL;
        maxx = maxy = 0;
        first = 0;
    }

    /* for continuous expose events, just keep track of the
       exposed areas and redraw once */
    minx = sx1 < minx ? sx1 : minx;
    maxx = sx2 > maxx ? sx2 : maxx;
    miny = sy1 < miny ? sy1 : miny;
    maxy = sy2 > maxy ? sy2 : maxy;

    if (cbs->event->xexpose.count == 0)
     /* this was the last in a string of exposes, redraw now */
    {
	/* make sure entire area is in canvas */
	minx = minx < 0 ? 0 : minx;
	miny = miny < 0 ? 0 : miny;
	maxx = maxx > screen_right ? screen_right : maxx;
	maxy = maxy > screen_bot ? screen_bot : maxy;
	
	/* save current drawing color */
	XGetGCValues (dpy, gc, GCForeground, &values);
	if (Box) /* we are in the middle of widening window */
	{
	    draw_expose_default();
	    first = 1;
	}
	
	/* there is a map open; redraw it */
	else if ((CM != NULL) && CM->head.orig_scale > 0)
	{
            screen_to_utm (minx, miny, &ux1, &uy1);
            screen_to_utm (maxx, maxy, &ux2, &uy2);

            set_draw_win (uy1, uy2, ux2, ux1);
	    if (ClearFirst)
		expose_erase( minx, miny, maxx - minx, maxy - miny);
	    else
    	        replot(CM);
	    display_win_elements();
	    outline_window();
            reset_draw_win();
	    highlight_lit_line();
	    first = 1;
        }
        else /* there is no map open; just erase */
        {
	    expose_erase( minx, miny, maxx - minx, maxy - miny);
	     first = 1;
        }
	/* reset current drawing color */
        XSetForeground (dpy, gc, values.foreground);
    }

}
expose_erase (x, y,  w, h)
    int x, y, w, h;
{
     XSetForeground (dpy, gc, dcolors[CLR_ERASE]);
     XDrawRectangle(dpy, XtWindow(canvas), gc, x, y, w, h);
}

display_lit_line()
{
    if (points && line)
    {
	    V2_read_line(CM, CPoints, line);
	    display_line (type, CPoints, line, CM);
    }
}
highlight_lit_line()
{
    if (points)
    {
	if (line)
	{
	    V2_read_line(CM, CPoints, line);
	    highlight_line (type, CPoints, line, CM);
	}
	else
	        plot_points( type, points->n_points, points->x, points->y, 
			     CLR_HIGHLIGHT, CLR_HIGHLIGHT);
    }
}
		    
remove_from_list(new)
{
    int i, j;

    for (i = 0; i < curr; i++)
    {
	if (new == display_list[i])
	{
	    for (j = i + 1; j < curr; j++)
		display_list[j-1] = display_list[j];
	    curr--;
	    break;
	}
    }
}
zero_display_list()
{
    int i;

    for (i = 0; i < curr; i++)
	display_list[i] = 0;
    ClearFirst = 0;
    curr = 0;
}
add_to_display_list (new)
    int new;
{
    if ( new == MWC_CLEAR) 
    {
	zero_display_list();
	ClearFirst = 1;
    }
    else if (new)
    {
	remove_from_list (new);
	display_list[curr] = new;
	curr++;
    }
    else
    {
	zero_display_list();
    }
}
display_win_elements()
{
    int i;
    if (!display_list[0])
	return(0);
    
    for (i = 0; i < curr; i++)
	win_men (NULL, display_list[i]);
    return (1);
}

set_lit_line(Points, t, n)
    struct line_pnts *Points;
    char t;
    int n;
{
    type = t;
    points = Points;
    line = n;
}
reset_lit_line()
{
    display_lit_line();
    points = NULL;
}
