
/* 
 * FILE: cursor.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTIONS:
 * 
 * DoBrushCursor()
 * Sets the cursor of the widget parameter to a 
 * brush symbol. 
 *
 * DoZoomCursor()
 * Sets the cursor of the widget parameter to a 
 * magnifying-glass symbol. 
 *
 * UndoCursor()
 * Resets the cursor of the widget parameter to 
 * the default cursor symbol.
 *
 */

#include "xgre.h"
#include "bmaps.h"

/*********************/
/*** DoBrushCursor ***/
/*********************/

void DoBrushCursor(shell)
Widget shell;
{
Display *display;
Screen  *screenPtr;
Window   window;
Colormap cmap;
Pixmap   brushPixmap;
Pixmap   brushmaskPixmap;
Cursor   brushCursor;
XColor   blackColor, whiteColor, exact;
Status   resBlack, resWhite;

#ifdef DEBUG
printf("DoBrushCursor:\n");
#endif

display   = XtDisplay(shell);
screenPtr = XtScreen(shell);
window    = XtWindow(shell);
cmap      = DefaultColormapOfScreen(screenPtr);

resBlack = XAllocNamedColor(display, cmap, "black", &exact, &blackColor);
resWhite = XAllocNamedColor(display, cmap, "white", &exact, &whiteColor);

brushPixmap = XCreateBitmapFromData(display,window,
   (char *)bcursor_bits,bcursor_width,bcursor_height);

brushmaskPixmap = XCreateBitmapFromData(display,window,
   (char *)bmask_bits,bmask_width,bmask_height);

brushCursor = XCreatePixmapCursor(display,brushPixmap,
   brushmaskPixmap,&blackColor,&whiteColor,bcursor_x_hot,bcursor_y_hot);

XDefineCursor(display, window, brushCursor);
XFlush(display);
}

/********************/
/*** DoZoomCursor ***/
/********************/

void DoZoomCursor(shell)
Widget shell;
{
Display *display;
Screen  *screenPtr;
Window   window;
Colormap cmap;
Pixmap   zoomPixmap;
Pixmap   zoommaskPixmap;
Cursor   zoomCursor;
XColor   blackColor, whiteColor, exact;
Status   resBlack, resWhite;

#ifdef DEBUG
printf("DoZoomCursor:\n");
#endif

display   = XtDisplay(shell);
screenPtr = XtScreen(shell);
window    = XtWindow(shell);
cmap      = DefaultColormapOfScreen(screenPtr);

resBlack = XAllocNamedColor(display, cmap, "black", &exact, &blackColor);
resWhite = XAllocNamedColor(display, cmap, "white", &exact, &whiteColor);

zoomPixmap = XCreateBitmapFromData(display,window,
   (char *)zcursor_bits,zcursor_width,zcursor_height);

zoommaskPixmap = XCreateBitmapFromData(display,window,
   (char *)zmask_bits,zmask_width,zmask_height);

zoomCursor = XCreatePixmapCursor(display,zoomPixmap,
   zoommaskPixmap,&blackColor,&whiteColor,zcursor_x_hot,zcursor_y_hot);

XDefineCursor(display, window, zoomCursor);
XFlush(display);
}

/******************/
/*** UndoCursor ***/
/******************/

void UndoCursor(shell)
Widget shell;
{
Display *display;
Window window;

#ifdef DEBUG
printf("UndoCursor:\n");
#endif

display = XtDisplay(shell);
window = XtWindow(shell);
XUndefineCursor(display, window);
XFlush(display);
}


