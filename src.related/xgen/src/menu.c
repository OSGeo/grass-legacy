/**********************************************************************
   menu.c       - create a menu shell
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/

/***************************************************************
 * This routine takes a menu description and creates it.
 **************************************************************/
#include "xgen.h"

void
CreateMenu(s, isDynamic)
    Shell                          *s;
    Boolean                         isDynamic;
{
    Resource                       *resource;   /* pointer to the current
                                                 * resource */
    Widget                          main_window; /* the MainWindow widget   */
    Widget                          menu_bar;   /* the Menubar widget       */
    Widget                          help;       /* the Help widget          */
    Widget                          frame;      /* the Frame widget         */
    Widget                          swindow = NULL; /* the ScrolledWindow widget*/
    Widget                          row_col;    /* the RowColumn widget     */
    Widget                          hsb, vsb;   /* the ScrollBar widgets    */
    Widget                          oneW;       /* the last object widget   */
    Atom                            protocol;
    Boolean                         width_specd, /* has the width been
                                                  * specified ?             */
                                    height_specd; /* has the height been
                                                   * specified ?            */
    int                             n;  /* the Arg counter                  */
    WidgetClass                     widgetClass; /* override or transient   */
    int                             popupWidth, popupHeight;
    int                             dpyWidth = DisplayWidth(xgenGD.display, 
							    xgenGD.screen);
    int                             dpyHeight = DisplayHeight(xgenGD.display, 
							      xgenGD.screen);
    int                             numChildren;        /* how many kids does
                                                         * our menu have ? */

    width_specd = height_specd = False;
    n = 0;
    SetGlobalArgs(&n, NOFONTS);
    SetShellColorArgs(s, &n);
    if ((resource = IndexResource((char *) s, SHELL, "dx")) != NULL) {
        int                             x;

        if (resource->variable)
            ExpandVariable(resource, (char *) s, SHELL);
        if (!IsPercent(resource->val.dval)) {
            sprintf(errorbuf, "dx value in shell [%s] out of range, ", s->name);
            sprintf(errorbuf, "%sit must be in the range [0.0,100.0]",
                    errorbuf);
            XgenWarning("create paned commandboard", errorbuf);
            resource->val.dval = 0.0;
        }
        x = (int) ((resource->val.dval / 100.0) * (double) dpyWidth);
        XtSetArg(args[n], XmNx, x);
        n++;
    }
    if ((resource = IndexResource((char *) s, SHELL, "dy")) != NULL) {
        int                             y;

        if (resource->variable)
            ExpandVariable(resource, (char *) s, SHELL);
        if (!IsPercent(resource->val.dval)) {
            sprintf(errorbuf, "dy value in shell [%s] out of range, ", s->name);
            sprintf(errorbuf, "%sit must be in the range [0.0,100.0]",
                    errorbuf);
            XgenWarning("create menu", errorbuf);
            resource->val.ival = 0.0;
        }
        y = (int) ((resource->val.dval / 100.0) * (double) dpyHeight);
        XtSetArg(args[n], XmNy, y);
        n++;
    }
    if ((resource = IndexResource((char *) s, SHELL, "width")) != NULL) {
        if (resource->variable)
            ExpandVariable(resource, (char *) s, SHELL);
        XtSetArg(args[n], XmNwidth, resource->val.ival);
        n++;
        width_specd = True;
    } else {
        XtSetArg(args[n], XmNwidth, 100);
        n++;
    }
    if ((resource = IndexResource((char *) s, SHELL, "height")) != NULL) {
        if (resource->variable)
            ExpandVariable(resource, (char *) s, SHELL);
        XtSetArg(args[n], XmNheight, resource->val.ival);
        n++;
        height_specd = True;
    } else {
        XtSetArg(args[n], XmNheight, 100);
        n++;
    }
    widgetClass = transientShellWidgetClass;
    if ((resource = IndexResource((char *) s, SHELL, "override")) != NULL) {
        widgetClass = overrideShellWidgetClass;
    }
    XtSetArg(args[n], XmNallowShellResize, True);
    n++;
    if ((resource = IndexResource((char *) s, SHELL, "decorations")) != NULL) {
	if (resource->variable)
	    ExpandVariable(resource, (char *) s, SHELL);
	if ( XmIsMotifWMRunning(xgenGD.applShell) ) {
	    unsigned int decor_flags;

	    decor_flags = ParseDecorations(resource->val.cval);
	    if ( decor_flags != 0 )
		XtSetArg(args[n], XmNmwmDecorations, decor_flags); n++;
	} else {
	    verbose = True;

	    XgenWarning("cannot effect shell decorations and/functions",
		"Motif window manager is not running");
	}
    }
    if ((resource = IndexResource((char *) s, SHELL, "functions")) != NULL) {
	if (resource->variable)
	    ExpandVariable(resource, (char *) s, SHELL);
	if ( XmIsMotifWMRunning(xgenGD.applShell) ) {
	    unsigned int func_flags;

	    func_flags = ParseFunctions(resource->val.cval);
	    if ( func_flags != 0 )
		XtSetArg(args[n], XmNmwmFunctions, func_flags); n++;
	} else {
            verbose = True;

            XgenWarning("cannot effect shell decorations and/functions",
                "Motif window manager is not running");
        }
    }
    if ((resource = IndexResource((char *) s, SHELL, "titlestring")) != NULL) {
        if (resource->variable)
            ExpandVariable(resource, (char *) s, SHELL);
        s->widget = XtCreatePopupShell(resource->val.cval,
                                    widgetClass, xgenGD.applShell, args, n);
    } else {
        s->widget = XtCreatePopupShell(s->name, widgetClass,
                                       xgenGD.applShell, args, n);
    }
    protocol = XmInternAtom(xgenGD.display,"WM_DELETE_WINDOW", False);
    XmAddWMProtocols(s->widget,&protocol, 1);
    XtAddEventHandler(s->widget, NoEventMask, True, XgenClientMessage, s);

    /***************************************************************
     * create the MainWindow : this is used since it allows for a
     * MenuBar, a Frame, and a ScrolledWindow (if necessary).
     * The application designer might choose to place labels, lists,
     * or pushbuttons in a menu.
     **************************************************************/
    n = 0;
    SetGlobalArgs(&n, NOFONTS);
    SetShellColorArgs(s, &n);
    main_window = XmCreateMainWindow(s->widget, s->name, args, n);
    XtManageChild(main_window);

    /***************************************************************
     * create the MenuBar with help button (if help is supplied)
     **************************************************************/
    menu_bar = NULL;
    if ((resource = IndexResource((char *) s, SHELL, "help")) != NULL) {
        n = 0;
        SetGlobalArgs(&n, NOFONTS);
        SetShellColorArgs(s, &n);
        menu_bar = XmCreateMenuBar(main_window, "menu_bar", args, n);
        XtManageChild(menu_bar);
        n = 0;
        SetGlobalArgs(&n, FONTS);
        SetShellColorArgs(s, &n);
        help = XmCreateCascadeButton(menu_bar, "Help", args, n);
        XtManageChild(help);
        /***************************************************************
          * add the help callback, passing it the help string
          **************************************************************/
        if (resource->variable)
            ExpandVariable(resource, (char *) s, SHELL);
        XtAddCallback(help, XmNactivateCallback, helpCB,
                      (caddr_t) resource->val.cval);

        n = 0;
        XtSetArg(args[n], XmNmenuHelpWidget, help);
        n++;
        XtSetValues(menu_bar, args, n);
    }
    /***************************************************************
     * create a frame for the objects
     **************************************************************/
    n = 0;
    SetGlobalArgs(&n, NOFONTS);
    SetShellColorArgs(s, &n);
    frame = XmCreateFrame(main_window, "frame", args, n);
    XtManageChild(frame);

    /***************************************************************
     * create a scrolled window
     **************************************************************/
    if (isDynamic || IndexResource((char *) s, SHELL, "visibleitems")) {
        n = 0;
        SetGlobalArgs(&n, NOFONTS);
        SetShellColorArgs(s, &n);
        XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmAS_NEEDED);
        n++;
        XtSetArg(args[n], XmNscrollingPolicy, XmAUTOMATIC);
        n++;
        XtSetArg(args[n], XmNvisualPolicy, XmCONSTANT);
        n++;
        swindow = XmCreateScrolledWindow(frame, "swindow", args, n);
        XtManageChild(swindow);
    }
    /***************************************************************
     * create a rowcolumn widget to manage the objects
     **************************************************************/
    n = 0;
    SetGlobalArgs(&n, NOFONTS);
    SetShellColorArgs(s, &n);
    XtSetArg(args[n], XmNpacking, XmPACK_COLUMN);
    n++;
    XtSetArg(args[n], XmNisAligned, False);
    n++;
    XtSetArg(args[n], XmNnumColumns, 1);
    n++;
    if (isDynamic || IndexResource((char *) s, SHELL, "visibleitems"))
        row_col = XmCreateRowColumn(swindow, "rc", args, n);
    else
        row_col = XmCreateRowColumn(frame, "rc", args, n);
    XtManageChild(row_col);

    /***************************************************************
     * set main window areas
     **************************************************************/
    XmMainWindowSetAreas(main_window, menu_bar, NULL, NULL, NULL, frame);
    XtRealizeWidget(s->widget);

    if (isDynamic || IndexResource((char *) s, SHELL, "visibleitems")) {
	Widget clipWindow;

        n = 0;
        XtSetArg(args[n], XmNhorizontalScrollBar, &hsb);
        n++;
        XtSetArg(args[n], XmNverticalScrollBar, &vsb);
        n++;
        XtSetArg(args[n], XmNclipWindow, &clipWindow);
        n++;
        XtGetValues(swindow, args, n);

        n = 0;
        SetShellColorArgs(s, &n);
        XtSetValues(vsb, args, n);
        XtSetValues(hsb, args, n);
        XtSetValues(swindow, args, n);
        XtSetValues(clipWindow, args, n);
    }
    /***************************************************************
     * if the shell has objects....set up the best width/height
     **************************************************************/
    numChildren = 0;
    if ((NULL != (oneW = CreateObject(s, row_col, isDynamic, &numChildren, False))) &&
          (isDynamic || IndexResource((char *) s, SHELL, "visibleitems"))) {
        XtWidgetGeometry                blank,  /* send in a blank geometry
                                                 * spec */
                                        preferred,      /* geometry spec
                                                         * returned */
                                        othergeo;       /* spare geometry spec
                                                         * returned */
        Dimension                       dpyht,  /* display height */
                                        dpywid, /* display width */
                                        spacing,        /* spacing in the
                                                         * ScrolledWindow */
                                        width,  /* the width of the
                                                 * ScrolledWindow */
                                        height; /* the height " " " */
        short                           sthick; /* the shadow thickness " " " */
        Boolean                         taller = False, /* taller then dpyht */
                                        wider = False;  /* wider then dpyht */

        /***************************************************************
         * get the preferred width and height of the row_col widget
         * if it's taller than DisplayHeight, make it slightly smaller
         * if it's wider than DisplayWidth, make it slightly smaller
         ***************************************************************/
        bzero((char *) &blank, sizeof(XtWidgetGeometry));
        blank.request_mode = XtCWQueryOnly;
        XtQueryGeometry(row_col, &blank, &preferred);
        width = preferred.width;
        height = preferred.height;
        if (height > (dpyht = dpyHeight)) {
            height = dpyht - 150;
            taller = True;
        }
        if (width > (dpywid = dpyWidth)) {
            width = dpywid - 100;
            wider = True;
        }
        n = 0;
        XtSetArg(args[n], XmNshadowThickness, &sthick);
        n++;
        XtSetArg(args[n], XmNspacing, &spacing);
        n++;

        XtGetValues(swindow, args, n);

        /***************************************************************
         * I know there must be a better way to do this, but I messed
         * around for too long trying to get it right.
         **************************************************************/
        if (NULL != (resource = IndexResource((char *) s, SHELL, "visibleitems"))) {
            double                          oneHeight, rcHeight, offsetHeight,
                                            kids;


            if (resource->variable)
                ExpandVariable(resource, (char *) s, SHELL);
            if (resource->val.ival <= numChildren) {
                blank.request_mode = CWHeight;
                bzero((char *) &blank, sizeof(XtWidgetGeometry));
                blank.request_mode = XtCWQueryOnly;
                XtQueryGeometry(oneW, &blank, &othergeo);
                oneHeight = (double) othergeo.height;
                rcHeight = (double) preferred.height;
                kids = (double) numChildren;
                offsetHeight = (rcHeight - (oneHeight * kids)) / kids;
                height = ((int) (oneHeight) * resource->val.ival) +
                    ((int) offsetHeight) * (resource->val.ival + 3);
                taller = True;
            }
        }
        /***************************************************************
         * if height is spec'd do nothing
         * if maxheight spec'd and is less than preferred, use it
         * else go with preferred value and nix the scrollbar
         **************************************************************/
        if (!height_specd) {
            if (((resource = IndexResource((char *) s, SHELL, "maxheight")) != NULL)) {
                if (resource->variable)
                    ExpandVariable(resource, (char *) s, SHELL);
                if ((Dimension) resource->val.ival < height)
                    height = resource->val.ival;
            }
        }
        /***************************************************************
         * if width is spec'd do nothing
          * if maxwidth spec'd and is less than preferred, use it
         * else go with preferred value and nix the scrollbar
          **************************************************************/
        if (!width_specd) {
            if (((resource = IndexResource((char *) s, SHELL, "maxwidth")) != NULL)) {
                if (resource->variable)
                    ExpandVariable(resource, (char *) s, SHELL);
                if ((Dimension) resource->val.ival < width)
                    width = resource->val.ival;
            }
        }
        /***************************************************************
         * do it
         **************************************************************/

        n = 0;
        if (taller) {
            bzero((char *) &blank, sizeof(XtWidgetGeometry));
            blank.request_mode = XtCWQueryOnly;
            XtQueryGeometry(vsb, &blank, &preferred);

            XtSetArg(args[n], XmNwidth, width + spacing +
                     sthick * 4 + preferred.width);
            n++;
            if (!wider) {
                XtSetArg(args[n], XmNheight, height);
                n++;
            }
        }
        if (wider) {
            bzero((char *) &blank, sizeof(XtWidgetGeometry));
            XtQueryGeometry(hsb, &blank, &preferred);

            XtSetArg(args[n], XmNheight, height + spacing +
                     sthick * 4 + preferred.height);
            n++;
            if (!taller) {
                XtSetArg(args[n], XmNwidth, width);
                n++;
            }
        }
        if (!wider && !taller) {
            XtSetArg(args[n], XmNheight, height + 2 * sthick);
            n++;
            XtSetArg(args[n], XmNwidth, width + 2 * sthick);
            n++;
        }
        XtSetValues(swindow, args, n);
        XmAddTabGroup(vsb);
    }
    n = 0;
    XtSetArg(args[n], XtNwidth, &popupWidth);
    n++;
    XtSetArg(args[n], XtNheight, &popupHeight);
    n++;
    XtGetValues(s->widget, args, n);

    n = 0;
    if ((resource = IndexResource((char *) s, SHELL, "x")) != NULL) {
        if (resource->variable)
            ExpandVariable(resource, (char *) s, SHELL);
        if (abs(resource->val.ival) > dpyWidth) {
            sprintf(errorbuf, "x value in shell [%s] out of range, ", s->name);
            sprintf(errorbuf, "%sthe screen is only %d pixels wide",
                    errorbuf, dpyWidth);
            XgenWarning("create menu", errorbuf);
            resource->val.ival = 0;
        }
        if (resource->val.ival < 0) {
            XtSetArg(args[n], XmNx, dpyWidth - popupWidth + resource->val.ival);
            n++;
        } else {
            XtSetArg(args[n], XmNx, resource->val.ival);
            n++;
        }
    }
    if ((resource = IndexResource((char *) s, SHELL, "y")) != NULL) {
        if (resource->variable)
            ExpandVariable(resource, (char *) s, SHELL);
        if (abs(resource->val.ival) > dpyHeight) {
            sprintf(errorbuf, "y value in shell [%s] out of range, ", s->name);
            sprintf(errorbuf, "%sthe screen is only %d pixels tall",
                    errorbuf, dpyHeight);
            XgenWarning("create menu", errorbuf);
            resource->val.ival = 0;
        }
        if (resource->val.ival < 0) {
            XtSetArg(args[n], XmNy, dpyHeight - popupHeight + resource->val.ival);
            n++;
        } else {
            XtSetArg(args[n], XmNy, resource->val.ival);
            n++;
        }
    }
    XtSetValues(s->widget, args, n);
}
