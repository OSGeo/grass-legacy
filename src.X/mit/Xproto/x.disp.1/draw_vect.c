/* Callback of command button "New Window" of main menu	*/

#ifndef         INCLUDE
#define         INCLUDE

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#endif

extern Window active_window;

void draw_vectors(new, client_data, call_data)
	Widget new;
	caddr_t client_data;
	caddr_t call_data;
{
	char buf[100];

	printf("\n vectors drawn");

	sprintf(buf, "Xvect %u roads_p &", active_window);
	system(buf);
}
