
/* drawpuck:
** this function draws a "puck" that is used for changing position
*/

#define PUCKSIZ 10

#include "interface.h"

void 
drawpuck (Widget w, data_cell *dc, caddr_t *call_data)
{
int x0, y0;

    x0 = dc->x - PUCKSIZ/2;
    y0 = dc->y - PUCKSIZ/2;

    XClearArea (dc->dpy, XtWindow (w), 0, 0, 0, 0, FALSE);
    XSetForeground (dc->dpy, dc->gc, BlackPixelOfScreen (XtScreen(w)));
    XDrawArc (dc->dpy, XtWindow (w), dc->gc, x0, y0, 
			        PUCKSIZ, PUCKSIZ,0, 360*64);

    XSetForeground (dc->dpy, dc->gc, WhitePixelOfScreen (XtScreen(w)));
    XDrawArc (dc->dpy, XtWindow (w), dc->gc, x0, y0, 
				PUCKSIZ, PUCKSIZ,45*64, 170*64);


}

/**********************************************************************/
