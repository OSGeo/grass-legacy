
/* trackmouse:
** track the mouse movement while inside of a positioner widget
*/

#include "interface.h"

void 
trackmouse (Widget w, data_cell *dc, XEvent *event)
{
    static XEvent event2;

    if(event) {
	dc->x = event2.xbutton.x = event->xbutton.x;
	dc->y = event2.xbutton.y = event->xbutton.y;
	drawpuck (w, dc, NULL);
    }

    else {
	dc->x = event2.xbutton.x;
	dc->y = event2.xbutton.y;
	drawpuck (w, dc, NULL);
    }

}
