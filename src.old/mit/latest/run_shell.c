
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

extern Window display_win;

void run_shell(draw, client_data, call_data)
	Widget draw;
	caddr_t client_data;
	caddr_t call_data;
{	
	char buf[100];
	sprintf(buf, "runaway %u", 
			display_win);
	system(buf);
}
