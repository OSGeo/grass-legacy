
/* installcolormap:
** this is the function that sets the GL colormap apart from the 
** colormap used by the X server
*/

#include "interface.h"



void 
installcolormap (data_cell *dc)
{
	Window windows[2];

	windows[0] = XtWindow(dc->monitor);
	windows[1] = XtWindow(dc->toplevel);
	XSetWMColormapWindows(XtDisplay(dc->toplevel), 
		                    XtWindow(dc->toplevel), windows,2);
}

