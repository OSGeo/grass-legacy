/*******************************************************************
 * Line drawing in the current window.
 *
 * Clip window:
 *   D_set_clip_window (top, bottom ,left, right)
 *      establish clipping region for subseqent line drawing.
 *   D_set_clip_window_to_map_window ()
 *     set clipping to pixels corresponding to the current map region
 *     (default)
 *
 * Moves.
 *   D_move_abs(x,y)   move to x,y.
 *   D_move_rel(x,y)   move to +x,+y.
 *      Set current position. Position is not clipped.
 *
 * Draw line 
 *   D_cont_abs(x,y)   draw to x,y.
 *   D_cont_rel(x,y)   draw to +x,+y.
 *      Line draw from current position. New postion is not clipped.
 *      The lines drawn are clipped however.
 *      Return values indicate the nature of the clipping:
 *        0 no clipping
 *        1 part of the line is drawn
 *       -1 none of the line is drawn
 *   
 *
 */
#include "raster.h"
#include "gis.h"
#include "display.h"

static int clip (void);
static int line_eq(int,int,int,int,int,int);

static int curx, cury;
static int left, right, top, bottom;     /* window edges */
static int x1,y1,x2,y2;

static int window_set = 0;

#define swap(x,y) {int t; t=x; x=y; y=t;}
#define limit(a,x,b) x<a?a:(x>b?b:x)

int D_set_clip_window (int Top, int Bottom, int Left, int Right)
{
/* make sure top is above bottom, left is left of right */
    if (Top > Bottom) swap (Top, Bottom);
    if (Left > Right) swap (Left, Right);

/* make sure edges are within the true window edges */
    D_get_screen_window (&top, &bottom, &left, &right);
    Top    = limit(top,Top,bottom);
    Bottom = limit(top,Bottom,bottom);
    Left   = limit (left, Left,right);
    Right  = limit (left, Right,right);

/* set the window */
    top     = Top;
    bottom  = Bottom;
    left    = Left;
    right   = Right;

    window_set = 1;

    R_move_abs (left, top);

    return 0;
}

int D_set_clip_window_to_map_window ()
{
    D_set_clip_window (
	(int)D_get_d_north(),
	(int)D_get_d_south(),
	(int)D_get_d_west(),
	(int)D_get_d_east()
		      ) ;

    return 0;
}

int D_cont_abs (int x,int y)
{
    int clipped;

    x1 = curx;
    y1 = cury;
    x2 = x;
    y2 = y;
    curx = x;
    cury = y;

    if (!window_set)
	D_set_clip_window_to_map_window ();

    clipped = clip();
    if (clipped >= 0)
    {
	R_move_abs (x1, y1);
	R_cont_abs (x2, y2);
    }

    return clipped;
}

int D_cont_rel (int x,int y)
{
    return D_cont_abs (curx+x, cury+y);
}

int D_move_abs (int x,int y)
{
    curx = x;
    cury = y;
    return 0;
}

int D_move_rel (int x,int y)
{
    curx += x;
    cury += y;
    return 0;
}

/*********************************************************************
 * this code is the window clipping for D_cont_abs()
 *********************************************************************/

#define Y(x)  line_eq(x,x0,y0,dx,dy,xround)

#define X(y)  line_eq(y,y0,x0,dy,dx,yround)


static int clip ()
{
    register int x0,y0;
    register int dx,dy;
    int xround;
    int yround;
    int clipped;

/*
 * quick check for line above,below,left,or right of window
 */
    if (x1 < left && x2 < left)
	return - 1;
    if (x1 > right && x2 > right)
	return -1;

    if (y1 < top && y2 < top)
	return -1;
    if (y1 > bottom && y2 > bottom)
	return -1;

/*
 * setup line equations variables
 */
    x0 = x1 ;
    y0 = y1 ;

    dx = x2 - x1;
    dy = y2 - y1;

    if ((xround = dx/2) < 0) xround = -xround;
    if ((yround = dy/2) < 0) yround = -yround;

/*
 * clipping
 *
 * if x of endpoint 1 doesn't fall within the window
 *    move x to the nearest edge
 *    recalculate the y
 *      if the new y doesn't fall within the window then
 *      the line doesn't cross into the window
 *
 * if y of endpoint 1 doesn't fall within the window
 *    move y to the nearest edge
 *    recalculate the x
 *      if the new x doesn't fall within the window then
 *      the line doesn't cross into the window
 *
 * repeat for the second endpoint
 *
 */

    clipped = 0;
    if (x1 < left || x1 > right)
    {
	if (dx == 0)
	    return -1;

	x1 = x1 < left ? left : right ;
	y1 = Y(x1);

	if (y1 < top || y1 > bottom)
	{
	    if (dy == 0)
		return -1;

	    y1 = y1 < top ? top : bottom ;
	    x1 = X(y1);

	    if (x1 < left || x1 > right)
		return -1;
	}
	clipped = 1;
    }
    if (y1 < top || y1 > bottom)
    {
	if (dy == 0)
	    return -1;
	y1 = y1 < top ? top : bottom ;
	x1 = X(y1);

	if (x1 < left || x1 > right)
	{
	    if (dx == 0)
		return -1;

	    x1 = x1 < left ? left : right ;
	    y1 = Y(x1);

	    if (y1 < top || y1 > bottom)
		return -1;
	}
	clipped = 1;
    }

    if (x2 < left || x2 > right)
    {
	if (dx == 0)
	    return -1;

	x2 = x2 < left ? left : right ;
	y2 = Y(x2);

	if (y2 < top || y2 > bottom)
	{
	    if (dy == 0)
		return -1;

	    y2 = y2 < top ? top : bottom ;
	    x2 = X(y2);

	    if (x2 < left || x2 > right)
		return -1 ;
	}
	clipped = 1;
    }
    if (y2 < top || y2 > bottom)
    {
	if (dy == 0)
	    return -1;

	y2 = y2 < top ? top : bottom ;
	x2 = X(y2);

	if (x2 < left || x2 > right)
	{
	    if (dx == 0)
		return -1;

	    x2 = x2 < left ? left : right ;
	    y2 = Y(x2);

	    if (y2 < top || y2 > bottom)
		return -1;
	}
	clipped = 1;
    }

    return clipped;
}

static int line_eq(int x,int x0,int y0,int dx,int dy,int round)
{
    register int t;

    if((t = dy*(x-x0)) < 0) t-= round;
    else t += round;;

    return (y0 + t / dx);
}
