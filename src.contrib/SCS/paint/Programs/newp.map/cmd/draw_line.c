#include "graphics.h"
#include "misc.h"

#define Y(x)  line_eq(x,x0,y0,dx,dy,xround)

#define X(y)  line_eq(y,y0,x0,dy,dx,yround)



draw_line (x1,y1,x2,y2)

{
	register int x0,y0;
	register int dx,dy;
	int xround;
	int yround;
	int left,right,top,bottom;
	int ttop;

	left = graphics.window.left - graphics.width1 - 1;
	right = graphics.window.right + graphics.width2 + 1;
	top = graphics.window.top - graphics.width1 - 1 ;
	bottom = graphics.window.bottom + graphics.width2 + 1 ;

/*
printf (" top is %d \n", top);
printf (" bottom is %d \n", bottom );
printf (" x1, is %d \n", x1);
printf (" y1, is %d \n", y1);
	set_color(BLACK);
	graph_point(x1+2, y1-10);
*/

	x0 = x1 ;

	y0 = y1 ;

#ifdef DEBUG
printf("\ndraw_line: (%d,%d)-(%d,%d)\n", x1,y1,x2,y2);
#endif

	if (x1 < left && x2 < left)
		return;
	if (x1 > right && x2 > right)
		return;

	if (y1 < top && y2 < top)
		return;
	if (y1 > bottom && y2 > bottom)
		return;

#ifdef FULLDEBUG
printf("x1(%d),y1(%d)  x2(%d),y2(%d)\n",x1,y1,x2,y2);
#endif

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

	if (x1 < left || x1 > right)
	{
#ifdef FULLDEBUG
printf("\tx1(%d) being clipped\n",x1);
#endif
		if (dx == 0)
			return;
		x1 = x1 < left ? left : right ;
		y1 = Y(x1);
#ifdef FULLDEBUG
printf("\tx1(%d), y1(%d)\n",x1,y1);
#endif

		if (y1 < top || y1 > bottom)
		{
#ifdef FULLDEBUG
printf("\t\ty1(%d) being clipped\n",y1);
#endif
			if (dy == 0)
				return;
			y1 = y1 < top ? top : bottom ;
			x1 = X(y1);
#ifdef FULLDEBUG
printf("\t\tx1(%d), y1(%d)\n",x1,y1);
#endif
			if (x1 < left || x1 > right)
				return;
		}
	}
	if (y1 < top || y1 > bottom)
	{
#ifdef FULLDEBUG
printf("\ty1(%d) being clipped\n",y1);
#endif
		if (dy == 0)
			return;
		y1 = y1 < top ? top : bottom ;
		x1 = X(y1);
#ifdef FULLDEBUG
printf("\ty1(%d), x1(%d)\n",y1,x1);
#endif

		if (x1 < left || x1 > right)
		{
#ifdef FULLDEBUG
printf("\t\tx1(%d) being clipped\n",x1);
#endif
			if (dx == 0)
				return;
			x1 = x1 < left ? left : right ;
			y1 = Y(x1);
#ifdef FULLDEBUG
printf("\t\ty1(%d), x1(%d)\n",y1,x1);
#endif
			if (y1 < top || y1 > bottom)
				return;
		}
	}

	if (x2 < left || x2 > right)
	{
#ifdef FULLDEBUG
printf("\tx2(%d) being clipped\n",x2);
#endif
		if (dx == 0)
			return;
		x2 = x2 < left ? left : right ;
		y2 = Y(x2);
#ifdef FULLDEBUG
printf("\tx2(%d), y2(%d)\n",x2,y2);
#endif

		if (y2 < top || y2 > bottom)
		{
#ifdef FULLDEBUG
printf("\t\ty2(%d) being clipped\n",y2);
#endif
			if (dy == 0)
				return;
			y2 = y2 < top ? top : bottom ;
			x2 = X(y2);
#ifdef FULLDEBUG
printf("\t\tx2(%d), y2(%d)\n",x2,y2);
#endif
			if (x2 < left || x2 > right)
				return ;
		}
	}
	if (y2 < top || y2 > bottom)
	{
#ifdef FULLDEBUG
printf("\ty2(%d) being clipped\n",y2);
#endif
		if (dy == 0)
			return;
		y2 = y2 < top ? top : bottom ;
		x2 = X(y2);
#ifdef FULLDEBUG
printf("\ty2(%d), x2(%d)\n",y2,x2);
#endif

		if (x2 < left || x2 > right)
		{
#ifdef FULLDEBUG
printf("\t\tx2(%d) being clipped\n",x2);
#endif
			if (dx == 0)
				return;
			x2 = x2 < left ? left : right ;
			y2 = Y(x2);
#ifdef FULLDEBUG
printf("\t\ty2(%d), x2(%d)\n",y2,x2);
#endif
			if (y2 < top || y2 > bottom)
				return;
		}
	}


/* graph the line */

#ifdef DEBUG
printf("graph line: (%d,%d),(%d,%d)\n",x1,y1,x2,y2);
#endif

	graph_line (x1,y1,x2,y2);
}
