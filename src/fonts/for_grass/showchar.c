#include <curses.h>

int skip;
int curx, cury;
static first_time = 1 ;

showchar (achar)
	char achar ;
{
    int n;
    unsigned char *X, *Y;
    int x,y;
    int i;

	get_char_vects(achar, &n, &X, &Y) ;

	if (first_time)
	{
		initscr();
		first_time = 0 ;
	}
    clear();
    printw ("character contains %d vectors", n);

/* left and right lines */
    x = X[0] - 'R';
    y = Y[0] - 'R';
    skip = 1;
    draw (x, -10); draw (x, 10);
    skip = 1;
    draw (y, -10); draw (y, 10);
    refresh();

/* draw character */
    skip = 1;
    for (i = 1; i < n; i++)
    {
	if (X[i] == ' ') skip = 1;
	else draw (X[i] - 'R', Y[i] - 'R');
    }
    move (LINES-1, 0);
    addch('\n');
    refresh();
    endwin();
}

draw (x,y)
{
    if (!skip)
	draw_line (curx+COLS/2, cury+LINES/2, x+COLS/2, y+LINES/2);
    skip = 0;
    curx = x;
    cury = y;
}

line_eq (x,x0,y0,dx,dy,round)
{
    register int t;

    if((t = dy*(x-x0)) < 0) t-= round;
    else t += round;;

    return (y0 + t / dx);
}
#define Y(x)  line_eq(x,x0,y0,dx,dy,xround)
#define X(y)  line_eq(y,y0,x0,dy,dx,yround)

#define LEFT 0
#define RIGHT (COLS-1)
#define TOP 0
#define BOTTOM (LINES-1)

draw_line (x1,y1,x2,y2)
{
    register int x0,y0;
    register int dx,dy;
    int xround;
    int yround;

    x0 = x1 ;

    y0 = y1 ;

    if (x1 < LEFT && x2 < LEFT)     return;
    if (x1 > RIGHT && x2 > RIGHT)   return;

    if (y1 < TOP && y2 < TOP)       return;
    if (y1 > BOTTOM && y2 > BOTTOM) return;


    dx = x2 - x1;
    dy = y2 - y1;

    if ((xround = dx/2) < 0) xround = -xround;
    if ((yround = dy/2) < 0) yround = -yround;

/**********************************************
* do clipping
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
**********************************************/

    if (x1 < LEFT || x1 > RIGHT)
    {
	if (dx == 0) return;
	x1 = x1 < LEFT ? LEFT : RIGHT ;
	y1 = Y(x1);

	if (y1 < TOP || y1 > BOTTOM)
	{
	    if (dy == 0) return;
	    y1 = y1 < TOP ? TOP : BOTTOM ;
	    x1 = X(y1);
	    if (x1 < LEFT || x1 > RIGHT) return;
	}
    }
    if (y1 < TOP || y1 > BOTTOM)
    {
	if (dy == 0) return;
	y1 = y1 < TOP ? TOP : BOTTOM ;
	x1 = X(y1);

	if (x1 < LEFT || x1 > RIGHT)
	{
	    if (dx == 0) return;
	    x1 = x1 < LEFT ? LEFT : RIGHT ;
	    y1 = Y(x1);
	    if (y1 < TOP || y1 > BOTTOM) return;
	}
    }

    if (x2 < LEFT || x2 > RIGHT)
    {
	if (dx == 0) return;
	x2 = x2 < LEFT ? LEFT : RIGHT ;
	y2 = Y(x2);

	if (y2 < TOP || y2 > BOTTOM)
	{
	    if (dy == 0) return;
	    y2 = y2 < TOP ? TOP : BOTTOM ;
	    x2 = X(y2);
	    if (x2 < LEFT || x2 > RIGHT) return ;
	}
    }
    if (y2 < TOP || y2 > BOTTOM)
    {
	if (dy == 0) return;
	y2 = y2 < TOP ? TOP : BOTTOM ;
	x2 = X(y2);

	if (x2 < LEFT || x2 > RIGHT)
	{
	    if (dx == 0) return;
	    x2 = x2 < LEFT ? LEFT : RIGHT ;
	    y2 = Y(x2);
	    if (y2 < TOP || y2 > BOTTOM) return;
	}
    }


/* graph the line */

    graph_line (x1,y1,x2,y2);
}
graph_line (x0,y0,x1,y1)
    register int x0, y0 ;
    int x1, y1 ;
{
    int dx, dy;
    int xinc, yinc;

    register res1;
    int res2;

    xinc = 1;
    yinc = 1;
    if ((dx = x1-x0) < 0) 
    {
	xinc = -1;
	dx = -dx;
    }
    if ((dy = y1-y0) < 0) 
    {
	yinc = -1;
	dy = -dy;
    }
    res1 = 0;
    res2 = 0;

    if (dx > dy)
	while (x0 != x1)
	{
	    graph_point (x0, y0);
	    if (res1 > res2)
	    {
		res2 += dx - res1;
		res1 = 0;
		y0 += yinc;
	    }
	    res1 += dy;
	    x0 += xinc;
	}
    else if (dx < dy)
	while (y0 != y1)
	{
	    graph_point (x0, y0);
	    if (res1 > res2)
	    {
		res2 += dy - res1;
		res1 = 0;
		x0 += xinc;
	    }
	    res1 += dx;
	    y0 += yinc;
	}
    else
	while (x0 != x1)
	{
	    graph_point (x0, y0);
	    y0 += yinc;
	    x0 += xinc;
	}

    graph_point (x1, y1);
}
graph_point (x,y)
{
    move (y,x);
    addch ('.');
}
