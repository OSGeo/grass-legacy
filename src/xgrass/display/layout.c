#include "xgdisp.h"

void
#ifdef _NO_PROTO
CreateLayout(shell)
    Widget shell;
#else
CreateLayout(Widget shell)
#endif
{
    Widget widget;
    Widget menuBar;
    Widget toolBar;
    Widget rowcolumn;
    Widget bb;
    Arg al[15];
    int ac = 0;
    Pixel white;
    XmString xms;
    XmString xxms;
    XmString yxms;
    int columns;


    XtSetArg(al[ac], XmNspacing, 4); ac++;
    XtSetArg(al[ac], XmNwidth, 500); ac++;
    XtSetArg(al[ac], XmNheight, 500); ac++;
    XtSetArg(al[ac], XmNvisual, Global.visual); ac++;
    Global.mainWindow = XmCreateMainWindow(shell,"main_window", al, ac);

    toolBar = CreateObjectButtonPad(Global.mainWindow, NULL, 3, XmHORIZONTAL);

    menuBar = CreateMenuBar(Global.mainWindow);

    Global.hWidth = XmConvertUnits(Global.applShell, XmHORIZONTAL, 
        Xm1000TH_INCHES, (int)(Global.pageWidth*1000.0), XmPIXELS);
    Global.vHeight = XmConvertUnits(Global.applShell, XmVERTICAL, 
        Xm1000TH_INCHES, (int)(Global.pageHeight*1000.0), XmPIXELS);

    Global.hsb = XtVaCreateManagedWidget("hsb", xmScrollBarWidgetClass, 
        Global.mainWindow,
        XmNorientation, XmHORIZONTAL,
        XmNminimum, 0, XmNmaximum, Global.hWidth,
        XmNprocessingDirection, XmMAX_ON_RIGHT, 
        XmNdragCallback, hsbCBList,
        XmNvalueChangedCallback, hsbCBList,
        NULL);

    Global.vsb = XtVaCreateManagedWidget("vsb", xmScrollBarWidgetClass, 
        Global.mainWindow,
        XmNorientation, XmVERTICAL,
        XmNminimum, 0, XmNmaximum, Global.vHeight,
        XmNprocessingDirection, XmMAX_ON_BOTTOM, 
        XmNdragCallback, vsbCBList,
        XmNvalueChangedCallback, vsbCBList,
        NULL);

    bb = XtVaCreateManagedWidget("bb",
        xmBulletinBoardWidgetClass, Global.mainWindow,
	XmNmarginWidth, 0,
	XmNmarginHeight, 0,
	XmNwidth, 500,
	XmNheight, 500,
	XmNallowOverlap, False,
        NULL);

    white = WhitePixel(Global.display,Global.screenNo);

    Global.hRulerWidget = XtVaCreateManagedWidget("hruler",
        xmDrawingAreaWidgetClass, bb,
	XmNbackground, white,
        XmNwidth, Global.hWidth, XmNheight, 15, 
        XmNx, 15, XmNy, 0,
        NULL);

    XtAddEventHandler(Global.hRulerWidget, 
        ExposureMask,
        False, HandleExposeEvents, NULL);

    Global.vRulerWidget = XtVaCreateManagedWidget("vruler",
        xmDrawingAreaWidgetClass, bb,
	XmNbackground, white,
        XmNwidth, 15, XmNheight, Global.vHeight, 
        XmNx, 0, XmNy, 15,
        NULL);

    XtAddEventHandler(Global.vRulerWidget, 
        ExposureMask,
        False, HandleExposeEvents, NULL);

    Global.scrollWindow = XtVaCreateManagedWidget("scrolled_window",
        xmScrolledWindowWidgetClass, bb,
        XmNscrollBarDisplayPolicy, XmSTATIC,
        XmNscrollingPolicy, XmAPPLICATION_DEFINED,
        XmNhorizontalScrollBar, Global.hsb,
        XmNverticalScrollBar, Global.vsb,
        XmNwidth, Global.hWidth + 15 + 4,
        XmNheight, Global.vHeight + 15 + 4,
        XmNx, 15, XmNy, 15,
        NULL);

    XtAddEventHandler(bb, 
        StructureNotifyMask,
        False, HandleResizeEvents, NULL);

    Global.drawArea = XtVaCreateManagedWidget("draw_area",
        xmDrawingAreaWidgetClass, Global.scrollWindow,
	XmNbackground, white,
        XmNwidth, Global.hWidth, XmNheight, Global.vHeight, 
	XmNresizePolicy, XmRESIZE_NONE,
        NULL);

    CreatePopup();

    XtVaSetValues(Global.scrollWindow, XmNworkWindow, Global.drawArea, NULL);

    XtAddEventHandler(Global.drawArea, 
        PointerMotionMask|ButtonPressMask|ButtonReleaseMask,
        False, HandleMouseEvents, XtGetMultiClickTime(Global.display));

    XtAddEventHandler(Global.drawArea, 
        ExposureMask,
        False, HandleExposeEvents, NULL);

    rowcolumn = XtVaCreateManagedWidget("message_window",
	xmRowColumnWidgetClass, Global.mainWindow,
        XmNcolumns, 1,
        XmNorientation, XmHORIZONTAL,
        XmNpacking, XmPACK_TIGHT,
        XmNadjustLast, True,
	NULL);

    if ( Global.units != XGD_UNITS_PIXELS ) {
	if ( Global.units == XGD_UNITS_INCHES ) {
            xxms = XmStringCreateSimple("X (in.):");
            yxms = XmStringCreateSimple("Y (in.):");
        } else {
            xxms = XmStringCreateSimple("X (mm.):");
            yxms = XmStringCreateSimple("Y (mm.):");
        }
        columns = 8;
    } else {
	xxms = XmStringCreateSimple("X:");
	yxms = XmStringCreateSimple("Y:");
        columns = 8;
    }
    Global.xPosAreaLabel = XtVaCreateManagedWidget("x",
        xmLabelWidgetClass, rowcolumn,
        XmNlabelString, xxms,
        NULL);
    Global.xPosArea = XtVaCreateManagedWidget("00000",
        xmTextFieldWidgetClass, rowcolumn,
        XmNeditable, False,
        XmNcolumns, columns,
        NULL);
    Global.yPosAreaLabel = XtVaCreateManagedWidget("y",
        xmLabelWidgetClass, rowcolumn,
        XmNlabelString, yxms,
        NULL);
    Global.yPosArea = XtVaCreateManagedWidget("00000",
        xmTextFieldWidgetClass, rowcolumn,
        XmNeditable, False,
        XmNcolumns, columns,
        NULL);

    XmStringFree(xxms);
    XmStringFree(yxms);

    widget = XtVaCreateManagedWidget("MODE:",
        xmLabelWidgetClass, rowcolumn,
        NULL);
    xms = XmStringCreateSimple(GetStringFromMode(Global.mode));
    Global.messageArea = XtVaCreateManagedWidget("mode",
        xmLabelWidgetClass, rowcolumn,
	XmNlabelString, xms,
        NULL);
    XmStringFree(xms);

    XtVaSetValues(Global.mainWindow, XmNmessageWindow, rowcolumn, 
        XmNshowSeparator, True, NULL);

    XmMainWindowSetAreas(Global.mainWindow, menuBar, toolBar, Global.hsb, 
        Global.vsb, bb);
    XtManageChild(Global.mainWindow);

}

