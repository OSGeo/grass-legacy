/*  %W%  %G%  */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include "options.h"
#include "gis.h"

Display* the_display;
Window the_window;
GC the_gc;
XGCValues the_gc_values;
unsigned long the_gc_valuemask;
int the_screen;
XWindowAttributes winattr;
XEvent report;
XImage *xi;

label()
{
	char achar[2] ;
	int SCREEN_X, SCREEN_Y ;
	int button ;
	char answer[64] ;
	int T, B, L, R;
	int t, b, l, r, row ;
	char *panel ;
	int tsize;

	unsigned int window_width, window_height,
                border_width, depth ;
        int x, y;
        Window root_return;


	/* Set the display to be the default display */
        if ((the_display = XOpenDisplay("")) == NULL)
        {
        printf(" can't open display\n");
        return(-1);
        }

        the_screen = DefaultScreen(the_display);

        the_window = XD_get_cur_window(the_display,
                                the_screen);

	XSelectInput(the_display, the_window,
                        ButtonPressMask);


        XGetWindowAttributes(the_display, the_window,
                &winattr);


        the_gc_values.line_width = 1;

        the_gc = XCreateGC(the_display, the_window,
                GCLineWidth, &the_gc_values);

        XGetGeometry(the_display, the_window,
                &root_return, &x, &y, &window_width,
                &window_height, &border_width, &depth);

	T = 0;
	B = window_height;
	L = 0;
	R = window_width;	

	dots_per_line = (int)(size/100.0 * 
				(float) window_height);
        tsize = (int)(.8 * (float)dots_per_line) ;
        Text_size(tsize, tsize) ;


	achar[1] = 000 ;

	panel = G_tempfile() ;

/*
	SCREEN_X = (L + R) / 2 ;
	SCREEN_Y = (T + B) / 2 ;
*/

	while(1)	
	{
		G_clear_screen() ;
		printf( "Move pointer to label location\n") ;
		printf( " BUTTON  MEANS\n") ;
		printf( "  Left    Place label here\n") ;
		printf( "  Right   No more labels\n") ;


		XNextEvent(the_display, &report);

/*
		R_get_location_with_pointer(&SCREEN_X, &SCREEN_Y, &button) ;

		R_move_abs(SCREEN_X, SCREEN_Y) ;
*/

	switch(((XButtonPressedEvent *)&report)->button)
		{
			case 1:
	SCREEN_X = ((XButtonPressedEvent *)&report)->x;
        SCREEN_Y = ((XButtonPressedEvent *)&report)->y;

	
				printf( "\nNow type in label >  ") ;
				gets(answer) ;

			/* Clear out area for text */
				Move_abs(SCREEN_X, SCREEN_Y) ;
				Get_text_box(answer, &t, &b, &l, &r) ;
				t = t - 2 ; if (t < T) t = T ;
				b = b + 2 ; if (b > B) b = B ;
				l = l - 2 ; if (l < L) l = L ;
				r = r + 2 ; if (r > R) r = R ;
			/* Save the panel */
	xi = XGetImage(the_display, the_window, 
		l + 1, t + 1, r - l - 1 , b - t - 1,
		AllPlanes, XYPixmap);
	
	XSetForeground(the_display, the_gc,
		XD_make_colr(the_display, the_window,
		the_screen,
		winattr.colormap, backcolor));

	XFillRectangle(the_display, the_window, the_gc,
		l + 1, t + 1, r - l - 1 , b - t - 1);


				/* Draw text */
				Move_abs(SCREEN_X, SCREEN_Y) ;
	XSetForeground(the_display, the_gc,
                XD_make_colr(the_display, the_window,
		the_screen,
                winattr.colormap, textcolor));

				Text(answer) ;
				XFlush(the_display);

				if (is_not_ok())
				{
			/*R_panel_restore(panel) ;*/
	XPutImage(the_display, the_window, the_gc,
	xi, 0, 0, l + 1, t + 1, r - l - 1 , b - t - 1);
				}
				else
				{
				SCREEN_Y = b + dots_per_line ;
				if (SCREEN_Y > B) SCREEN_Y = B ;
				}
				break ;

			case 2:
				break ;

			case 3:
				XDestroyImage(xi);
				return(0) ;
				break ;
		}
	}
}

static
is_not_ok()
{
	char answer[64] ;

	for(;;)
	{
		printf("\nIs this OK?  y/n : ") ;
		gets(answer) ;
		switch(answer[0] & 0177)
		{
		case 'y':
		case 'Y':
			return(0) ;
		case 'n':
		case 'N':
			return(1) ;
		default:
			break ;
		}
	}
}
