#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include "gis.h"

double e_minus_w, n_minus_s;
struct Cell_head window;
extern Display* the_display;
extern Window display_win;
char n_str[12], s_str[12],
	w_str[12], e_str[12],
	nsres_str[12], ewres_str[12];
Atom north, south, east, west, ns, ew;

void attach_window_info()
{
	void set_win_info();

	G_gisinit("junk");
	G_get_window(&window);

	sprintf(n_str,"%10.2lf", window.north);
	sprintf(s_str,"%10.2lf", window.south);
	sprintf(w_str,"%10.2lf", window.west);
	sprintf(e_str,"%10.2lf", window.east);
	sprintf(nsres_str,"%10.2lf", window.ns_res);
	sprintf(ewres_str,"%10.2lf", window.ew_res);

	north = XInternAtom(the_display, "north", False);
	south = XInternAtom(the_display, "south", False);
	west = XInternAtom(the_display, "west", False);
	east = XInternAtom(the_display, "east", False);
	ns = XInternAtom(the_display, "nsres", False);
	ew = XInternAtom(the_display, "ewres", False);

	set_win_info();

}

void set_win_info()
{

	XChangeProperty(the_display, display_win, north,
			XA_STRING, 8, PropModeReplace,
			n_str, 12);

	XChangeProperty(the_display, display_win, south,
			XA_STRING, 8, PropModeReplace,
			s_str, 12);

	XChangeProperty(the_display, display_win, west,
			XA_STRING, 8, PropModeReplace,
			w_str, 12);

	XChangeProperty(the_display, display_win, east,
			XA_STRING, 8, PropModeReplace,
			e_str, 12);

	XChangeProperty(the_display, display_win, ns,
			XA_STRING, 8, PropModeReplace,
			nsres_str, 12);

	XChangeProperty(the_display, display_win, ew,
			XA_STRING, 8, PropModeReplace,
			ewres_str, 12);

	sscanf(n_str, "%10.2lf", &window.north);
	sscanf(s_str, "%10.2lf", &window.south);
	sscanf(w_str, "%10.2lf", &window.west);
	sscanf(e_str, "%10.2lf", &window.east);
	sscanf(nsres_str, "%10.2lf", &window.ns_res);
	sscanf(ewres_str, "%10.2lf", &window.ew_res);

	e_minus_w = window.east - window.west;
	n_minus_s = window.north - window.south;	





	XFlush(the_display);
	printf("\n attached ");
}
