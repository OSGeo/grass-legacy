/* Callback of button "Choose Window" of main menu	*/

#ifndef         INCLUDE
#define         INCLUDE

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#endif

int choose_flag = 0;

void choose_window (choose, client_data, call_data)
	Widget choose;
	caddr_t client_data;
	caddr_t call_data;
{
	choose_flag = 1;
}
