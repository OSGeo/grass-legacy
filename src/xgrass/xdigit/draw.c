/*
**  Written by Terry Baker Winter 1993
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "color.h"

#define HUGE_VAL 1000000


static int currx, curry;

    
void
fill_pix()
{
    XCopyArea (XtDisplay (canvas), XtWindow (canvas), pix, gc,
               0, 0, screen_right, screen_bot, 0, 0);
}

move_rel (x, y)
    int x, y;
{
    currx += x;
    curry += y;
}

move_abs (x, y)
    int x, y;
{
    currx = x;
    curry = y;
}

cont_abs (x, y)
    int x, y;
{
int minx, miny, w, h;

    minx = x < currx? x : currx;
    miny = y < curry? y : curry;
    w = abs(currx -x);
    h = abs(curry -y);
    XDrawLine (dpy, XtWindow (canvas), gc, currx, curry, x, y);
    currx = x;
    curry = y;
}

cont_rel (x, y)
    int x, y;
{
int minx, miny, w, h;

    minx = x < 0? currx +x : currx;
    miny = y < 0? curry +y : curry;
    XDrawLine (dpy, XtWindow (canvas), gc, currx, curry, currx + x, curry + y);
    currx += x;
    curry += y;
}

set_canvas (w)
    Widget w;
{
    canvas = w;
    dpy = XtDisplay (w);
    gc = XCreateGC(dpy, RootWindow(dpy,DefaultScreen(dpy)), 0 , NULL);
    XSetPlaneMask (dpy, gc, AllPlanes );
}

draw_string (str)
    char *str;
{
    XDrawString (dpy, XtWindow(canvas), gc, currx, curry, str, strlen(str));
}
init_graphics()
{
   Dimension width, height;

    if (XtIsRealized (canvas))
    {
         screen_top = screen_left = 0;
        XtVaGetValues (canvas, 
	      XtNheight, &height,
	      XtNwidth, &width,
	      NULL);
	      screen_bot = (int)height;
	      screen_right = (int)width;
        if (pix)
            XFreePixmap (dpy, pix);
        pix = XCreatePixmap (dpy, DefaultRootWindow (dpy), width, height, 
                               DefaultDepthOfScreen (XtScreen (canvas)));
        XSetForeground (dpy, gc, dcolors[CLR_ERASE]);
        XFillRectangle(dpy, pix, gc, 0, 0, width, height);
    }


 }

standard_color (color)
    unsigned long  color;
{
    XSetForeground (dpy, gc, color);
}

translate_color (color)
    char *color;
{
    if (!strcmp(color, "yellow"))
	return(dcolors[XD_YELLOW]);
    if (!strcmp(color, "orange"))
	return(dcolors[XD_ORANGE]);
    if (!strcmp(color, "brown"))
	return(dcolors[XD_BROWN]);
    if (!strcmp(color, "red"))
	return(dcolors[XD_RED]);
    if (!strcmp(color, "magenta"))
	return(dcolors[XD_MAGENTA]);
    if (!strcmp(color, "violet"))
	return(dcolors[XD_VIOLET]);
    if (!strcmp(color, "blue"))
	return(dcolors[XD_BLUE]);
    if (!strcmp(color, "aqua"))
	return(dcolors[XD_AQUA]);
    if (!strcmp(color, "green"))
	return(dcolors[XD_GREEN]);
    if (!strcmp(color, "grey"))
	return(dcolors[XD_GREY]);
    if (!strcmp(color, "white"))
	return(dcolors[XD_WHITE]);
    if (!strcmp(color, "black"))
	return(dcolors[XD_BLACK]);
    
    return(dcolors[XD_WHITE]);
}

erase_window()
{
    XSetForeground (dpy, gc, dcolors[CLR_ERASE]);
    XFillRectangle(dpy, XtWindow(canvas), gc, 0, 0, screen_right, screen_bot);

}
copy_pix()
{
    XCopyArea (XtDisplay (canvas), pix, XtWindow (canvas), gc,
    		0, 0, screen_right, screen_bot, 0, 0);
}
void 
resize()
{
    XtVaGetValues (Mainform,XmNheight, &Hght, XmNwidth, &Wdth,
                          XmNx, &Winx ,XmNy, &Winy, NULL);

   if (CM)
   {
       init_graphics(); 
       setup_win();
       redraw();
    }
    
}

get_location_with_pointer(nx, ny, button)
    int *nx, *ny;
    int *button;
{
    int i = 0;
    do{
        i = Check_for_action(nx, ny);
    }while ((!i) || (i == DRAW));
    *button = i;
    return (i);
}

get_location_with_line(x, y, nx, ny, button)
    int x, y;
    int *nx, *ny;
    int *button;
{
    int i;
    int newx, newy;
    int oldx, oldy;
    int width, height;
    int tmpx, tmpy;
    int first_time = 1;
	      
    
    if ( !pix)
	init_graphics();
    
    XSetForeground (dpy, gc, dcolors[CLR_HIGHLIGHT]);
    i = 0;
    
    do { 
        i = Check_for_action(&newx, &newy);
	if (i)
	{
	    if (first_time)
	    {
		first_time = 0;
		oldx = newx;
		oldy = newy;
		fill_pix();
	    }
	    else
		if (newx == oldx && newy == oldy)
		    continue;

	    width = x - oldx; 
	    height= y - oldy; 
            if (width < 0)
	    {
		tmpx = x;
		width = -width;
	    }
	    else 
		tmpx = oldx;
            
	    if (height< 0)
	    {
		tmpy = y;
	        height = -height;
	    }
	    else 
		tmpy = oldy;
            XCopyArea (dpy, pix, XtWindow (canvas), gc,
           	    tmpx, tmpy, width+10, height+10, tmpx, tmpy);
	    if (i == DRAW)
            {
		XDrawLine (dpy, XtWindow (canvas), gc, x, y, newx, newy);
	        oldx = newx;
	        oldy = newy;
	    }
	}
    }while ((!i) || (i == DRAW));
    XCopyArea (dpy, pix, XtWindow (canvas), gc,
               0, 0, screen_right, screen_bot, 0, 0);
   *nx = newx;
   *ny = newy;
   *button = i; 
   return (i);
}

get_location_with_box(x, y, nx, ny, button)
    int *x, *y;
    int *nx, *ny;
    int *button;
{
    int i;
    Window win = XtWindow (canvas);
    int newx = 0, newy = 0;	/* to shut up saber */
    int width, height;
    int tmpx, tmpy;
    int oldx, oldy;
    int first_time = 1;
	      
    XSetForeground (dpy, gc, dcolors[CLR_HIGHLIGHT]);
    
    i = 0;
 
    /* newx and newy get initialized before 'i' becomes non-zero */
    do{
        i = Check_for_action(&newx, &newy);
	if ((i == DRAW))
	{
	    if (first_time)
	    {
               if (*button == FIND)
                   XCopyArea (dpy, pix, XtWindow (canvas), gc,
                             0, 0, screen_right, screen_bot, 0, 0);
		first_time = 0;
		tmpx = oldx = newx;
		tmpy = oldy = newy;
                width = height = 0;
    		fill_pix();
		Editing = 1;
	    }
	    else
		if (newx == tmpx && newy == tmpy)
		    continue;
            XCopyArea (dpy, pix, XtWindow (canvas), gc,
           	    tmpx, tmpy, width+10, height+10, tmpx, tmpy);;
	    width = oldx - newx; 
	    height= oldy - newy; 
            if (width < 0)
	    {
	         tmpx = oldx;
                 width = -width;
	    }
	    else 
		tmpx = newx;
            
	    if (height< 0)
	    {
	       tmpy = oldy;
                height = -height;
	    }
	    else 
		tmpy = newy;
            XDrawRectangle(dpy, XtWindow(canvas), 
			  gc, tmpx, tmpy, width, height);
	}
    } while ((!i) || (i == DRAW));
    if (i == DONE)
    {
    	XCopyArea (dpy, pix, XtWindow (canvas), gc,
               0, 0, screen_right, screen_bot, 0, 0);
	Editing = 0;
    }
    else if (i == ACCEPT)
	Editing = 0;
    else if (i == FIND) 
    {
       *x = tmpx;
       *y = tmpy;
       *nx = tmpx + width;
       *ny = tmpy + height;
    }
   *button = i;
   return (i);
}
polyline_abs (points, n, pcolor)
   XPoint *points;
   int n, pcolor;
{
    XDrawLines (dpy, XtWindow (canvas), gc, points, n, CoordModeOrigin);
    if (pcolor)
    {
	standard_color (dcolors[pcolor]);
        XDrawPoints(dpy, XtWindow (canvas), gc, points, n, CoordModeOrigin);
    }
}
polygon_abs (points, n)
   XPoint *points;
   int n;
{
    XFillPolygon 
	 (dpy, XtWindow (canvas), gc, points, n, Convex, CoordModeOrigin);
}
dot(x, y)
   int x, y;
{
    XDrawPoint (dpy, XtWindow (canvas), gc, x, y);
}
