
#include "driverlib.h"
#include "includes.h"

void
Respond(void)
{
    XClearWindow(dpy, grwin);
    XSync(dpy, 1);
    needs_flush = 0;
}

