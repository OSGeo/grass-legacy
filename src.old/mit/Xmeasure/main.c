/*  %W%  %G%  */

/*
 *   Xmeasure
 *
 *   Usage:  Xmeasure [forecolor] [backcolor]
 *
 *   Linear and area measure
 */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include "gis.h"
#include "driver.h"
#include <stdio.h>
#define USAGE   "[forecolor=name] [backcolor=name]"

Display *the_display;
Window the_window;
Colormap colormap;
int the_screen;
GC the_gc, drawXorGC;
XEvent report;

main(argc, argv)
        int argc ;
        char **argv ;
{
	char buffer[64] ;
	double area, new_area ;
	double atan() ;
	double calc_area() ;
	double cell_size ;
	double cur_ux, cur_uy ;
	double first_ux, first_uy ;
	double XD_d_to_u_row(), XD_d_to_u_col() ;
	double hypot() ;
	double length ;
	double sq_meters ;
	double ux, uy ;
	double x, y ;
	int button ;
	int cur_screen_x, cur_screen_y ;
	int first_x, first_y ;
	int screen_x, screen_y ;
	struct Cell_head window ;
	int t, b, l, r ;

	Window root_return;
        unsigned int x1, y1, window_width, window_height,
		border_width, depth;
	XWindowAttributes win_atts;

	/* Initialize the GIS calls */
        G_gisinit("Xmeasure") ;

/* Check command line */
        if (argc > 3)
        {
	fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE);
	exit(-1) ;
        }

        /* Set the display to be the default display */
        if ((the_display = XOpenDisplay("")) == NULL)
        {
        printf(" can't open display\n");
        return(-1);
        }

        the_screen = DefaultScreen(the_display);

        the_window = XD_get_cur_window(the_display,
                                the_screen);

	the_gc = XCreateGC(the_display, the_window,
                        0, None);

	drawXorGC = XCreateGC(the_display, the_window,
                        0, None);

	XSetFunction(the_display, drawXorGC, GXinvert);

        XSelectInput(the_display, the_window,
                        ButtonPressMask);


        XGetGeometry(the_display, the_window,
                &root_return, &x1 , &y1, &window_width,
                &window_height, &border_width, &depth);

	G_get_window(&window) ;

	if (XD_do_conversions(&window,
                window_width, window_height))
        G_fatal_error("Error in calculating conversions");


	for(;;)
	{
		G_clear_screen() ;
		printf( "\nButtons:\n") ;
		printf( "Left:   where am i\n") ;
		printf( "Middle: set FIRST vertice\n") ;
		printf( "Right:  quit this\n") ;


		while(1)	
		{
		XNextEvent(the_display, &report);

		if (report.type == ButtonPress)
         		{
        screen_x = ((XButtonPressedEvent *)&report)->x;
        screen_y = ((XButtonPressedEvent *)&report)->y;

		cur_uy = XD_d_to_u_row((double)screen_y)  ;
		cur_ux = XD_d_to_u_col((double)screen_x)  ;

		printf( "EAST: %10.2f\n", cur_ux) ;
		printf( "NORTH: %10.2f\n", cur_uy) ;

	if (((XButtonPressedEvent *)&report)->button == 3)
                exit(-1);

	if (((XButtonPressedEvent *)&report)->button == 2)
                break;

			} /* if over button */
		} 


		G_clear_screen() ;
		printf( "\nMiddle: set NEXT vertice\n") ;
		printf( "Right:  FINISH\n") ;

		Move_abs(screen_x, screen_y) ;
		first_ux = cur_ux ;
		first_uy = cur_uy ;
		first_x = screen_x ;
		first_y = screen_y ;
		cur_screen_x = screen_x ;
		cur_screen_y = screen_y ;

		area = 0. ;
		length = 0. ;



		do
		{
		RubberBand(the_window, &cur_screen_x,
		&cur_screen_y, &screen_x, &screen_y, &button);

		uy = XD_d_to_u_row((double)screen_y)  ;
		ux = XD_d_to_u_col((double)screen_x)  ;

		switch (button)
		{
		case 1:
			printf( "EAST:  %10.2f\n", ux) ;
			printf( "NORTH: %10.2f\n", uy) ;
			break ;
		case 2:
			black_and_white_line(screen_x,
			screen_y,cur_screen_x,cur_screen_y)  ;
			new_area = calc_area(cur_ux, cur_uy, ux, uy) ;
				area += new_area ;
				length += hypot(cur_ux - ux, cur_uy - uy) ;
				printf("LEN:   %10.2f\n", length) ;
				cur_screen_x = screen_x ;
				cur_screen_y = screen_y ;
				cur_ux = ux ;
				cur_uy = uy ;
				break ;
		default:
				break ;
			}
		} while (button != 3) ;


		new_area = calc_area(cur_ux, cur_uy, first_ux, first_uy) ;
		area += new_area ;

		if (area < 0.)
			area *= -1.0 ;

		G_clear_screen() ;
		printf( "\nButtons:\n") ;
		printf( "Left:   DO ANOTHER\n") ;
		printf( "Middle: \n") ;
		printf( "Right:  quit this\n") ;
/*
 * 10000 is sq meters per hectare
 * 2589988 is sq meters per sq mile
 */
		printf("\nLEN:   %10.2f\n", length) ;
		printf("      %10.2f hectares\n", area / 10000 ) ;
		printf("      %10.4f sq. miles\n", area / 2589988.11 ) ;

		while(1){
		XNextEvent(the_display, &report);

                if (report.type == ButtonPress)
		{
	if (((XButtonPressedEvent *)&report)->button == 3)
                exit(-1);
	else break;
		
		}
		}
		
	}
}

double
calc_area(x1, y1, x2, y2) 
	double x1, y1, x2, y2 ;
{
	return( (double)((x1-x2) * (y1+y2) / 2. )) ;
}


RubberBand( w, ix1, iy1, ix2, iy2, btn)
     Window w;
     int *ix1, *iy1, *ix2, *iy2, *btn;
{
  XEvent event;
  XWindowAttributes oldAttributes;    /* just to put attrib
s. */
  long oldEventMask;                  /* old mask to restor
e  */

  /* Start Getting Moved Events */

  XGetWindowAttributes(the_display, w, &oldAttributes);
  oldEventMask = oldAttributes.your_event_mask;
  XSelectInput(the_display,w, ((unsigned long) oldEventMask) |
                     PointerMotionMask);


  /* Setup and Draw First Line */
  *ix2 = *ix1;
  *iy2 = *iy1;
  XDrawLine(the_display,w,drawXorGC, *ix1, *iy1, *ix2, *iy2);

  /* Start Rubber Banding the Line until Click */

  while( 1 ) {

/*    XSync(the_display, 1);  Not Needed for Rubber Banding a Sing
le Line */
    XNextEvent(the_display, &event);

    /* -- Button Press Events indicates the end of motion -
- */
    if( event.type == ButtonPress ){

	*btn = ((XButtonPressedEvent *)(&event))->button;
      XSelectInput(the_display,w,(unsigned long) oldEventMask);
      if( ((XButtonPressedEvent *)(&event))->window != w)
        XPutBackEvent(the_display, &event);
      /* erase the line so that further process can be done
 */
      XDrawLine(the_display,w,drawXorGC,*ix1,*iy1,*ix2,*iy2);
	return;
    }
    /* -- Motion Events -- */
    else if( event.type == MotionNotify) {
      /* -- Motion Events in Structure Window -- */
      if ( ((XPointerMovedEvent *)(&event))->window == w) 
	{
        XDrawLine(the_display,w,drawXorGC,*ix1,*iy1,*ix2,*iy2);
        *ix2 = ((XPointerMovedEvent *)(&event))->x;
        *iy2 = ((XPointerMovedEvent *)(&event))->y;
        XDrawLine(the_display,w,drawXorGC,*ix1,*iy1,*ix2,*iy2);
      	}
      /* -- Motion Event on Another Window -- */
      else {
        XSelectInput(the_display ,w,(unsigned long) oldEventMask);
        XPutBackEvent(the_display, &event);
        return(-1);
      }
    }
    else {
        /* if (verbose) */
        printf("\nUnknown Event Detected in MoveNode\n");
      XSelectInput( the_display,w, ((unsigned long) oldEventMask))
;
      XPutBackEvent( the_display, &event);
      return(-1);
    }
  }
}
/********* end of function "RubberBand"	******************/



black_and_white_line(screen_x, screen_y,
			cur_screen_x,cur_screen_y)
        int screen_x,screen_y,cur_screen_x,cur_screen_y ;
{
	XSetForeground(the_display, the_gc,
		WhitePixel(the_display, the_screen));
        Move_abs(cur_screen_x, cur_screen_y) ;
        Cont_abs(screen_x, screen_y) ;

	XSetForeground(the_display, the_gc,
                BlackPixel(the_display, the_screen));

        if(abs(screen_y-cur_screen_y) 
			<= abs(screen_x-cur_screen_x))
        {
	Move_abs(cur_screen_x, cur_screen_y-1) ;
	Cont_abs(screen_x, screen_y-1) ;
        }
        else
        {
	Move_abs(cur_screen_x+1, cur_screen_y) ;
	Cont_abs(screen_x+1, screen_y) ;
        }

        XFlush(the_display) ;
}

