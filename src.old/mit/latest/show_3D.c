#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

extern Window display_win;

void show_3D(show, client_data, call_data)
	Widget show;
	caddr_t client_data;
	caddr_t call_data;
{
	char buf[100];

	printf("\n 3D View");

	sprintf(buf, "X.3d %u", display_win);
	system(buf);
}
