#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

extern Display *dpy;
extern int scrn;
extern Widget top_level;

int pixel(color_str)
char *color_str;
{
    XColor screen, exact;
    int status;


    status = XAllocNamedColor(dpy,
        DefaultColormap(dpy, scrn),
        color_str, &screen, &exact);

    if (status == 0)
        printf("\n failed");
    return (screen.pixel);
}
