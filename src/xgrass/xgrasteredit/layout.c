
/* 
 * FILE: layout.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * FUNCTION:
 *
 * CreateLayout(Widget shell) 
 * --------------------------
 * This functions creates the widget layout for the raster
 * editor.  It creates the main-window, menu-bar, button-bar
 * message-area widgets, and scrolling drawable window widgets.  
 * It hooks in the event handlers HandleMouseEvents() and 
 * HandleExposeEvents() (in handler.c).  It calls 
 * CreateMenuBar() (in menubar.c) and CreateButtonBar() 
 * (in buttonbar.c) to help with this do this and it wraps 
 * up by calling the Motif function XmMainWindowSetAreas() 
 * to make sure that everything is laid out properly. 
 *
 */

#include "xgre.h"

Widget CreateMessageArea();

/********************/
/*** CreateLayout ***/
/********************/

void
#ifdef _NO_PROTO
CreateLayout(shell)
   Widget shell;
#else
CreateLayout(Widget shell)
#endif
{
    Arg al[15];
    int ac = 0;
    Pixel white = WhitePixel(Global.display, Global.screenNo);
    Widget widget;
    Widget menuBar;
    Widget buttonBar;
    Widget messageArea;
    Widget hsb,vsb;
    Widget bb;
    int columns;
    XmString xms;
    XmString xxms;
    XmString yxms;

/*** MAIN WINDOW, MENU-BAR, BUTTON-BAR AND MESSAGE-AREA ***/

    XtSetArg(al[ac], XmNspacing, 4); ac++;
    XtSetArg(al[ac], XmNwidth, 500); ac++;
    XtSetArg(al[ac], XmNheight, 650); ac++;
    XtSetArg(al[ac], XmNvisual, Global.visual); ac++;
    Global.mainWindow = XmCreateMainWindow(shell,"main_window", al, ac);
    XtManageChild(Global.mainWindow);

    menuBar =     CreateMenuBar(Global.mainWindow);
    buttonBar =   CreateButtonBar(Global.mainWindow);
    messageArea = CreateMessageArea(Global.mainWindow);

/*** SCROLL WINDOW AND IMAGE AREA ***/

    Global.imageWidth = 500;  /* these change in loadimage.c */ 
    Global.imageHeight = 500; 

    Global.scrollWindow = XtVaCreateManagedWidget("scrolled_window",
        xmScrolledWindowWidgetClass, Global.mainWindow,
	XmNvisualPolicy, XmCONSTANT,
	XmNscrollBarDisplayPolicy, XmSTATIC,
	XmNscrollingPolicy, XmAUTOMATIC,NULL);

    Global.imageArea = XtVaCreateManagedWidget("image_area",
        xmDrawingAreaWidgetClass, Global.scrollWindow,
	XmNbackground, white,
	XmNwidth,500,
	XmNheight,500,NULL);
    XtAddEventHandler(Global.imageArea, 
       PointerMotionMask|ButtonPressMask|ButtonReleaseMask,
       False, HandleMouseEvents, XtGetMultiClickTime(Global.display));
    XtAddEventHandler(Global.imageArea, 
       ExposureMask,False,HandleExposeEvents, NULL);

    XgAddHelpCallBackFromFile(Global.imageArea,"xgre/xgre_image_area");

/*** MANAGE MAIN WINDOW ***/ 

    XtVaSetValues(Global.mainWindow,XmNmessageWindow,messageArea, 
        XmNshowSeparator, True, NULL);
    XmMainWindowSetAreas(Global.mainWindow,menuBar,buttonBar,
        NULL,NULL,Global.scrollWindow);

    XtManageChild(Global.mainWindow);
}

