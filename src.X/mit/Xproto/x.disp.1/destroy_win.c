/* Callback of button "Destroy Window" of main menu	*/

#ifndef         INCLUDE
#define         INCLUDE

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#endif

int destroy_flag = 0;

void destroy_window (close, client_data, call_data)
	Widget close;
	caddr_t client_data;
	caddr_t call_data;
{
	printf("\n  window destroyed");
	destroy_flag = 1;
}



void Destroyed(widget, closure, callData)
    	Widget widget;              /* unused */
	caddr_t closure;            /* unused */
	caddr_t callData;           /* unused */
{
	printf( "everything now destroyed.  Bye!\n" );
	exit(0);
}
