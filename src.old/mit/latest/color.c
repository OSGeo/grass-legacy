#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

extern Display *the_display;
extern int the_screen;
extern Widget top_level;

int  pixel(color_str)
	char *color_str;
{
	XColor screen, exact;
	int status;


	status = XAllocNamedColor(the_display,
	DefaultColormap(the_display, the_screen),
		color_str, &screen, &exact);

	if(status == 0) printf("\n failed");
	return(screen.pixel);
}

