#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/StringDefs.h>

Cursor cur1, cross_hair;
extern Display *the_display;

create_cursors()
{

	cur1 = XCreateFontCursor(the_display, 86);
	cross_hair = XCreateFontCursor(the_display, 34);
}
