#include "xgrass_lib.h"
#include "hourglass.h"

XgDoHourGlass(shell)
Widget shell;
{
    Display *display;
    Screen *screenPtr;
    Window window;
    Colormap cmap;
    Pixmap hourglassPixmap;
    Pixmap hourglassmaskPixmap;
    Cursor hourglassCursor;
    XColor blackColor, whiteColor, exact;
    Status resBlack, resWhite;

    display = XtDisplay(shell);
    screenPtr = XtScreen(shell);
    window = XtWindow(shell);
    cmap = DefaultColormapOfScreen(screenPtr);

    resBlack = XAllocNamedColor(display, cmap, "black", &exact, &blackColor);
    resWhite = XAllocNamedColor(display, cmap, "white", &exact, &whiteColor);

    hourglassPixmap = XCreateBitmapFromData(display, window,
                   (char *)hourglass_bits, hourglass_width, hourglass_height);
    hourglassmaskPixmap = XCreateBitmapFromData(display, window,
                   (char *)hourglassmask_bits, hourglassmask_width, 
                    hourglassmask_height);
    hourglassCursor = XCreatePixmapCursor(display, hourglassPixmap,
                   hourglassmaskPixmap, &blackColor, &whiteColor,
                   hourglass_x_hot, hourglass_y_hot);

    XDefineCursor(display, window, hourglassCursor);
    XFlush(display);
}

XgUndoHourGlass(shell)
Widget shell;
{
    Display *display;
    Window window;
    
    display = XtDisplay(shell);
    window = XtWindow(shell);
    XUndefineCursor(display, window);
}
