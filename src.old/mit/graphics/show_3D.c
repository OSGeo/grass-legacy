/* Callback of command button "New Window" of main menu	*/

#ifndef         INCLUDE
#define         INCLUDE

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#endif

extern Window active_window;

void show_3D(show, client_data, call_data)
	Widget show;
	caddr_t client_data;
	caddr_t call_data;
{
	char buf[100];

	printf("\n 3D View");

	sprintf(buf, "X.3d %u", active_window);
	system(buf);
}
