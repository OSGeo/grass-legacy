/* Callback of command button "New Window" of main menu	*/

#ifndef         INCLUDE
#define         INCLUDE

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#endif

extern Widget top_level;
extern Widget shell;
extern Widget display_win[50];

extern int no_disp_wins;

void quit(qt, client_data, call_data)
	Widget qt;
	caddr_t client_data;
	caddr_t call_data;
{
	int i;

	XtDestroyWidget((Widget) top_level);
	
	/*
	for(i=0; i<= no_disp_wins; i++)
		{
	if(display_win[i] != NULL)
	XtDestroyWidget(display_win[i]);
		}
		*/

}

