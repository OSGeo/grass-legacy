#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h> 
#include <X11/Xatom.h>
#include "gis.h"


#include "icon"

/* standard C include file */
#include <stdio.h>

Display *the_display;
Window the_window, root_window;
int the_screen;
XSizeHints size_hints;
XEvent the_event;
XClientMessageEvent *event_ptr;
Colormap standard;

Pixmap disp_icon;

#define START_X 20
#define START_Y 10
#define BORDER_WIDTH    3
#define MIN_WIDTH       550
#define MIN_HEIGHT      450


char vect_name[32];
char cell_name[32];
char overlay_name[32];
char font_name[32];
char color_mode[32];
char buf[100];
char window_name[32];

int n_vects = 0;


main(argv, argc)
        int argc;
        char **argv;
{

	Atom cell, overlay, font, colormode, vect; 
	XSetWindowAttributes attributes;
	char* display_name = NULL;
        char buf_str[100];
	int display_width, display_height;
	int window_width, window_height;
	XColor answer, screen, exact;
	Colormap make_fixed_colormap(), load_vect_colrs(),
			make_float_clr_table();
	int i, flags, no_cells;	
	FILE *fpr, *fopen();
	Status result;
        struct Cell_head window;
        Window root_return;
        unsigned int x, y, border_width, depth;
	int screen_x, screen_y;
        double east, north;
	double xl, xr, yt, yb;

	Window p_win;
	Atom actual_type;
	unsigned long nitems, remaining;
	unsigned char *one, *two, *three;
	int actual_format;

	G_gisinit("Xnew");

        /* connection to X server */
        if((the_display = XOpenDisplay(display_name)) == NULL)
	{
	fprintf(stderr,"Xnew : cannot connect to X server %s\n",
			XDisplayName(display_name));
	exit(-1);
	}

	/* getting screen size from display structure macro */
	the_screen = DefaultScreen(the_display);

	display_width = DisplayWidth(the_display, the_screen);
	display_height = DisplayHeight(the_display, the_screen);


	/* sizing of window */
	window_width = display_width/2.5;
	window_height = display_height/2.5;

        G_get_window(&window);

	root_window = RootWindow(the_display, the_screen);


	/* window creation */
	the_window = XCreateSimpleWindow(the_display, 
		root_window,
		START_X, START_Y, window_width, window_height,
		BORDER_WIDTH, BlackPixel(the_display, the_screen),
		BlackPixel(the_display, the_screen));	

	disp_icon = XCreateBitmapFromData
          (the_display, the_window, icon_bits, 64,58);

	/* initialization of size hint property for wm  */
	size_hints.flags = PSize | PPosition ;
	size_hints.x = START_X;
	size_hints.y = START_Y;
	size_hints.width = window_width;
	size_hints.height = window_height;


	/* properties for window manager   
	XSetStandardProperties(the_display, the_window,
		"grass_window", "icon", disp_icon,
		argv, argc, (XSizeHints *) &size_hints);
	*/

	XSetIconName(the_display, the_window,
			"GRASS_DISPLAY");
	

	/* selection of event types */
	XSelectInput(the_display, the_window,
		ExposureMask | ButtonPressMask |
		StructureNotifyMask |
		PropertyChangeMask);


	XMapWindow(the_display, the_window);

	/*printf("\n window_id = %u", the_window);*/


	/* setting backing store, colormap and save under attributes */


	no_cells = DisplayCells(the_display, the_screen);

	standard = XCreateColormap(the_display,the_window,
                DefaultVisual(the_display, the_screen),
                AllocAll);

	standard = make_fixed_colormap(standard, no_cells);


	for(i= 0; i < 256; i++)
	{
	answer.pixel = (unsigned long) i;
	XQueryColor(the_display, standard, &answer);
	}


	attributes.backing_store = Always;
        attributes.save_under = True;
        XChangeWindowAttributes(the_display,
                the_window, 
		CWBackingStore | CWSaveUnder ,
                &attributes);


	XSetWindowColormap(the_display, the_window,
                                        standard);


	strcpy(window_name, "Xnew");
	XStoreName(the_display, the_window, 
			window_name);

	/* set properties */
	strcpy(cell_name, "None");
	strcpy(vect_name, "None");
	strcpy(overlay_name, "None");
	strcpy(font_name, "");
	strcpy(color_mode, "fixed");


	cell = XInternAtom(the_display, "cell", False);
	vect = XInternAtom(the_display, "vect", False);
	overlay = XInternAtom(the_display,"overlay", False);
	font = XInternAtom(the_display, "font", False);
	colormode=XInternAtom(the_display,"colormode",False);

	XChangeProperty(the_display, the_window, cell,
		XA_STRING, 8, PropModeReplace,
		cell_name, 32);

	XChangeProperty(the_display, the_window, vect,
                XA_STRING, 8, PropModeReplace,
                vect_name, 32);

	XChangeProperty(the_display, the_window, overlay,
                XA_STRING, 8, PropModeReplace,
                overlay_name, 32);


	XChangeProperty(the_display, the_window, font,
                XA_STRING, 8, PropModeReplace,
                font_name, 32);

	XChangeProperty(the_display, the_window, colormode,
                XA_STRING, 8, PropModeReplace,
                color_mode, 32);




	/* event handling */
	while (1)
	{
	XNextEvent(the_display, &the_event);


	switch(the_event.type)
	{
	case ButtonPress:
		/* printf("\n ButtonPress"); */
                if ( the_event.xbutton.button == Button1 ) {
        		XGetGeometry(the_display, the_window,
                		&root_return, &x, &y, &window_width,
                		&window_height, &border_width, &depth);


        		XD_get_screen_bounds(&xl, &xr, &yt, &yb,
                		window.north, window.south,
                		window.west, window.east,
                		window_width, window_height);

        		screen_x = ((XButtonPressedEvent *)&the_event)->x;
        		screen_y = ((XButtonPressedEvent *)&the_event)->y;
		
		
        		east =  window.west + (screen_x - xl)
                		* (window.east - window.west) / (xr - xl);
		
        		north = window.north - (screen_y - yt)
                		* (window.north - window.south)/(yb - yt);

        		printf("%-13s  %-13s","EAST:","NORTH:") ;
                        printf("%13.2lf %13.2lf\n",east,north);
                        sprintf(buf_str,"%13.2lf %13.2lf",east,north);
                        XStoreBytes(the_display,buf_str,strlen(buf_str));

                }
		break;

	case ColormapNotify:
		/* printf("\n changed colors"); */
		break;

	case ConfigureNotify:
		break;

	case Expose:
		/* printf("\n Expose"); */

		while (XCheckTypedEvent(the_display,
			Expose, &the_event))
			;

		p_win = XD_get_cur_window(the_display,
				the_screen);

		if(p_win != the_window) break;

		XGetWindowProperty(the_display, 
			the_window, cell, 0, 320,
			False, XA_STRING, &actual_type,
			&actual_format, &nitems, 
			&remaining, &one);
	
		sscanf(one, "%s", cell_name);

		if(strcmp("None", cell_name))
                {
                /* printf("\n name = %s", cell_name); */
                sprintf(buf, "Xcell %s", cell_name);
                system(buf);
                }

		XGetWindowProperty(the_display,
                        the_window, overlay, 0, 320,
                        False, XA_STRING, &actual_type,
                        &actual_format, &nitems,
                        &remaining, &two);

		sscanf(two, "%s", overlay_name);


		if(strcmp("None", overlay_name))
                {
                printf("\n name = %s", overlay_name);
                sprintf(buf, "Xoverlay %s", overlay_name);
                system(buf);
                }

		XGetWindowProperty(the_display,
                        the_window, vect, 0, 320,
                        False, XA_STRING, &actual_type,
                        &actual_format, &nitems,
                        &remaining, &three);

                sscanf(three, "%s", vect_name);


                if(strcmp("None", vect_name))
                {
               sprintf(buf, "Xvect %s white", vect_name);
                system(buf);
                }


		/* printf("\n expose over"); */
		XFlush(the_display);


		break;

	case PropertyNotify:
		/*printf("\n property changed");*/
		break;

	case ClientMessage:
		/*printf("\n received a client message\n");*/
		event_ptr=(XClientMessageEvent *) &the_event;
		/*printf("map = %s\n", 
			(char *) (event_ptr->data.b));*/

		if(!strcmp("fixed",
			 (char *) (event_ptr->data.b)))
		standard = make_fixed_colormap(standard,
					no_cells);

		else if(!strcmp("float",
			 (char *) (event_ptr->data.b)))
		standard = load_vect_colrs(standard);

		else
		standard = make_float_clr_table(standard,
			(char *) (event_ptr->data.b));
		break;

	default:
		break;
	}	/* end of switch */
	}	/* end of while  */

}	/* end of MAIN program */

/*---------------------------------------------------------*/
