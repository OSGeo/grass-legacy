#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include "gis.h"

Widget top_level;
extern Widget display, outline, coors;
Window display_win, coor_win;
Display *the_display;
int the_screen;

GC  coor_gc;

int disp_width, disp_height;



char cell_displayed[25], *vector_files[5];
int n_vects = 0;
double xl, xr, yt, yb;

main(argc, argv)
	int argc;
	char *argv[];
{
	extern void create_display(), attach_window_info();
	XSetWindowAttributes attributes;
	register i;
	Drawable drawable_root;
	int x, y, border_width, drawable_depth;
	extern struct Cell_head window;
	XGCValues the_GC_values;
	unsigned long the_GC_value_mask;



	for(i = 0; i < 5; i++)
	vector_files[i] = NULL;

	/* X toolkit initialization and creation	*/ 
	/* of the top level widget			*/
	top_level = XtInitialize("graphics",
				 "Demo",
				 "NULL",
				 0,
				 &argc, argv);

	the_display = XtDisplay(top_level);
	the_screen  = DefaultScreen(the_display);

	create_display();
	
	printf("\n yuck0");
	XtRealizeWidget(outline);


	display_win = XtWindow(display);
	coor_win    = XtWindow(coors);	

	attach_window_info();

	/* calc window bounds inside the display win	*/
	XGetGeometry(the_display, display_win,	
			&drawable_root,&x,&y,&disp_width,
			&disp_height,&border_width,
			&drawable_depth);

	XD_get_screen_bounds(&xl, &xr, &yt, &yb,
		window.north, window.south,
		window.west, window.east,
		disp_width, disp_height);


	
	
	
		
	coor_gc = XCreateGC(the_display, coor_win, 
			None, &the_GC_values);

	XSetForeground(the_display, coor_gc, 1);






	attributes.backing_store = Always;
	attributes.save_under = True;
	XChangeWindowAttributes(the_display,
		display_win, CWBackingStore | CWSaveUnder,
		&attributes);


	XtMainLoop();
}
