#include "xgdisp.h"

Widget
#ifdef _NO_PROTO
CreateMenuBar(shell)
    Widget shell;
#else
CreateMenuBar(Widget shell)
#endif
{
    Widget menuBar;
    Widget fileMenu;
    Widget toolsMenu;
    Widget mapsMenu;
    Widget modesMenu;
    Widget attributesMenu;
    Widget optsMenu;
    Widget helpMenu;
    Widget cascade;
    Widget widget;
    XmString xms;

    menuBar = XmCreateMenuBar(shell, "menu_bar", NULL, 0);

    /* FILE MENU */
    fileMenu = XmCreatePulldownMenu(menuBar, "_pulldown", NULL, 0);

    xms = XmStringCreateSimple("File");
    cascade = XtVaCreateManagedWidget("file",
        xmCascadeButtonGadgetClass, menuBar,
        XmNsubMenuId, fileMenu,
        XmNlabelString, xms,
        XmNmnemonic, 'F',
        NULL);
    XmStringFree(xms);

	/*** save ***/
    widget = XtVaCreateManagedWidget("Open...", xmPushButtonWidgetClass,
        fileMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, OpenFileCallBack, 
        (XtPointer)NULL);

	/*** save ***/
    widget = XtVaCreateManagedWidget("Save...", xmPushButtonWidgetClass,
        fileMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, SaveObjectsCallBack, (XtPointer)NULL);

    widget = XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass,
        fileMenu, NULL);

	/*** Window dump ***/

    widget = XtVaCreateManagedWidget("Window dump...", xmPushButtonWidgetClass,
        fileMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, WindowDumpCallBack, NULL);

    widget = XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass,
        fileMenu, NULL);


	/*** quit ***/
    widget = XtVaCreateManagedWidget("Quit", xmPushButtonWidgetClass,
        fileMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, PBAct, (XtPointer)"Quit");


    /* END FILE MENU */

    /* Modes Menu */
    modesMenu = XmCreatePulldownMenu(menuBar, "_pulldown", NULL, 0);

    xms = XmStringCreateSimple("Modes");
    cascade = XtVaCreateManagedWidget("modes",
        xmCascadeButtonGadgetClass, menuBar,
        XmNsubMenuId, modesMenu,
        XmNlabelString, xms,
        XmNmnemonic, 's',
        NULL);
    XmStringFree(xms);

	/*** set in select mode ***/
    widget = XtVaCreateManagedWidget("Activate Select Mode", 
        xmPushButtonWidgetClass,
        modesMenu, 
	XmNvisibleWhenOff, True,
	NULL);
    XtAddCallback(widget, 
        XmNactivateCallback, TAct, XGD_MODE_SELECT);

    /* set Poly Reshape Mode */
    widget = XtVaCreateManagedWidget("Activate Poly Reshape Mode", 
        xmPushButtonWidgetClass,
        modesMenu, 
	XmNvisibleWhenOff, True,
	NULL);
    XtAddCallback(widget, 
        XmNactivateCallback, TAct, XGD_MODE_POLYRESHAPE);
    

	/*** set in highlight mode ***/
    widget = XtVaCreateManagedWidget("Activate Highlight Mode", 
        xmPushButtonWidgetClass,
        modesMenu, 
	XmNvisibleWhenOff, True,
	NULL);
    XtAddCallback(widget, 
        XmNactivateCallback, TAct, XGD_MODE_HIGHLIGHT);

	/*** set in modify color mode ***/
    widget = XtVaCreateManagedWidget("Activate Modify Colors Mode", 
        xmPushButtonWidgetClass,
        modesMenu, 
	XmNvisibleWhenOff, True,
	NULL);
    XtAddCallback(widget, 
        XmNactivateCallback, TAct, XGD_MODE_MODIFY_COLORS);

    
    /* Attributes Menu */
    attributesMenu = XmCreatePulldownMenu(menuBar, "_pulldown", NULL, 0);

    xms = XmStringCreateSimple("Attributes");
    cascade = XtVaCreateManagedWidget("attributes",
        xmCascadeButtonGadgetClass, menuBar,
        XmNsubMenuId, attributesMenu,
        XmNlabelString, xms,
        XmNmnemonic, 'A',
        NULL);
    XmStringFree(xms);
    
	/*** set grid attributes ***/
    widget = XtVaCreateManagedWidget("Set Grid Attributes...", 
        xmPushButtonWidgetClass, attributesMenu, NULL);

    XtAddCallback(widget, XmNactivateCallback, DoGridBox, NULL);

	/*** set legend attributes ***/
    widget = XtVaCreateManagedWidget("Set Legend Attributes...", 
        xmPushButtonWidgetClass, attributesMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, LegendBoxCB, NULL);

	/*** set bar scale attributes ***/
    widget = XtVaCreateManagedWidget("Set Scale Attributes...", 
        xmPushButtonWidgetClass, attributesMenu, NULL);

    XtAddCallback(widget, XmNactivateCallback, DoBarBox, NULL);

	/*** set label attributes ***/
    widget = XtVaCreateManagedWidget("Set Label Attributes...", 
        xmPushButtonWidgetClass, attributesMenu, NULL);

    XtAddCallback(widget, XmNactivateCallback, LabelAttCB, NULL);
    

	/*** set vector attributes ***//*
	  
    widget = XtVaCreateManagedWidget("Set Vector Attributes...", 
        xmPushButtonWidgetClass, attributesMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, PBAct, (XtPointer)"VectorAttr");

	*/

	/*** set icon standard ***/
    widget = XtVaCreateManagedWidget("Set Standard Site Attributes ...", 
        xmPushButtonWidgetClass, attributesMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, PBAct, (XtPointer)"SitesAttr");

    /* TOOLS MENU */
    toolsMenu = XmCreatePulldownMenu(menuBar, "_pulldown", NULL, 0);

    xms = XmStringCreateSimple("Tools");
    cascade = XtVaCreateManagedWidget("tools",
        xmCascadeButtonGadgetClass, menuBar,
        XmNsubMenuId, toolsMenu,
        XmNlabelString, xms,
        XmNmnemonic, 'T',
        NULL);
    XmStringFree(xms);

	/*** graphics tool box ***/
    widget = XtVaCreateManagedWidget("Graphic Tool Box...", 
        xmPushButtonWidgetClass, toolsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, DoToolBox, NULL);

    widget = XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass,
        toolsMenu, NULL);

	/*** set region for selected gframe ***/
    widget = XtVaCreateManagedWidget(
        "Set Region for Selected GeoFrame...", 
        xmPushButtonWidgetClass, toolsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, RegionCallBack, NULL);

    widget = XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass,
        toolsMenu, NULL);

	/*** set highlght color ***/
    widget = XtVaCreateManagedWidget(
        "Set Highlight Color...", 
        xmPushButtonWidgetClass, toolsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, SetHighlightColor, NULL);

    widget = XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass,
        toolsMenu, NULL);

	Global.setSite = XGD_SITE_STANDARD;

	/*** set icon pixmap ***/
    widget = XtVaCreateManagedWidget("Set Default Pixmap Site Symbol ...", 
        xmPushButtonWidgetClass, toolsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, PBAct, (XtPointer)"SitePixmap");

    widget = XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass,
        toolsMenu, NULL);

	/*** set default font ***/
    widget = XtVaCreateManagedWidget("Set Default Font...", 
        xmPushButtonWidgetClass, toolsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, SetFontCallBack, XGD_DEFAULT);

    /* END TOOLS MENU */

    /* MAPS MENU */
    mapsMenu = XmCreatePulldownMenu(menuBar, "_pulldown", NULL, 0);

    xms = XmStringCreateSimple("Maps");
    cascade = XtVaCreateManagedWidget("maps",
        xmCascadeButtonGadgetClass, menuBar,
        XmNsubMenuId, mapsMenu,
        XmNlabelString, xms,
        XmNmnemonic, 'M',
        NULL);
    XmStringFree(xms);

	/*** raster ***/
    widget = XtVaCreateManagedWidget("Raster...", xmPushButtonWidgetClass,
        mapsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, MapDisplayCallBack, XG_RASTER);

    /*** delete raster ***/
    widget = XtVaCreateManagedWidget("Delete Raster", xmPushButtonWidgetClass,
				     mapsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, DeleteMapCallBack, XG_RASTER);
    
	/*** vector ***/
    widget = XtVaCreateManagedWidget("Vector...", xmPushButtonWidgetClass,
        mapsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, MapDisplayCallBack, XG_VECTOR);

    /*** delete vector ***/
    widget = XtVaCreateManagedWidget("Delete Vector...",
				     xmPushButtonWidgetClass, mapsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, DeleteMapCallBack, XGD_VECTOR);
    
	/*** sites ***/
    widget = XtVaCreateManagedWidget("Sites...", xmPushButtonWidgetClass,
        mapsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, MapDisplayCallBack, XG_SITE);

    /*** delete site ***/
    widget = XtVaCreateManagedWidget("Delete Site...",
				     xmPushButtonWidgetClass, mapsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback,  DeleteMapCallBack, XGD_SITE);
    
    /* END MAPS MENU */

    /* OPTIONS MENU */
    optsMenu = XmCreatePulldownMenu(menuBar, "_pulldown", NULL, 0);

    xms = XmStringCreateSimple("Options");
    cascade = XtVaCreateManagedWidget("opts",
        xmCascadeButtonGadgetClass, menuBar,
        XmNsubMenuId, optsMenu,
        XmNlabelString, xms,
        XmNmnemonic, 'O',
        NULL);
    XmStringFree(xms);

        /*** units pixels ***/
    widget = XtVaCreateManagedWidget("Set Position Readout Units To Pixels", 
        xmPushButtonWidgetClass, optsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, SetUnits, XGD_UNITS_PIXELS);

        /*** units inches ***/
    widget = XtVaCreateManagedWidget("Set Position Readout Units To Inches", 
        xmPushButtonWidgetClass, optsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, SetUnits, XGD_UNITS_INCHES);

        /*** units millimeters ***/
    widget = XtVaCreateManagedWidget(
        "Set Position Readout Units To Millimeters", 
        xmPushButtonWidgetClass, optsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, SetUnits, XGD_UNITS_MILLI);

    widget = XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass,
        optsMenu, NULL);

	
	/*** to set vector file to be changed or deleted */
/*    widget = XtVaCreateManagedWidget ("Set One Vector file ",
	xmPushButtonWidgetClass, optsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, chlist, VECTOR);

	*//*** to set site file to be changed or deleted *//*
    widget = XtVaCreateManagedWidget ("Set One Site file ",
	xmPushButtonWidgetClass, optsMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, chlist, SITE);

    widget = XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass,
        optsMenu, NULL);
*/
	/*** set in what rast ***/
    widget = XtVaCreateManagedWidget("Activate Query Raster Mode", 
        xmPushButtonWidgetClass,
        optsMenu, 
	XmNvisibleWhenOff, True,
	NULL);
    XtAddCallback(widget, 
        XmNactivateCallback, TAct, XGD_MODE_QUERY_RASTER);

	/*** set in what vect ***/
    widget = XtVaCreateManagedWidget("Activate Query Vector Mode", 
        xmPushButtonWidgetClass,
        optsMenu, 
	XmNvisibleWhenOff, True,
	NULL);
    XtAddCallback(widget, 
        XmNactivateCallback, TAct, XGD_MODE_QUERY_VECTOR);

/*
    widget = XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass,
        optsMenu, NULL);
*/


	/*** set in zoom ***/
/*    widget = XtVaCreateManagedWidget("Activate Zoom Mode", 
        xmPushButtonWidgetClass,
        optsMenu, 
	XmNvisibleWhenOff, True,
	NULL);
    XtAddCallback(widget, 
        XmNactivateCallback, TAct, XGD_MODE_ZOOM);*/

    /* END OPTIONS MENU */

    /* HELP MENU */
    helpMenu = XmCreatePulldownMenu(menuBar, "_pulldown", NULL, 0);

    xms = XmStringCreateSimple("Help");
    cascade = XtVaCreateManagedWidget("help",
        xmCascadeButtonGadgetClass, menuBar,
        XmNsubMenuId, helpMenu,
        XmNlabelString, xms,
        XmNmnemonic, 'H',
        NULL);
    XmStringFree(xms);
 
    XtVaSetValues(menuBar, XmNmenuHelpWidget, cascade, NULL);

	/*** general ***/
    widget = XtVaCreateManagedWidget("General...", xmPushButtonWidgetClass,
        helpMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, PBAct, (XtPointer)"GeneralHelp");

	/*** current mode ***/
    widget = XtVaCreateManagedWidget("Current Mode...", xmPushButtonWidgetClass,
        helpMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, PBAct, (XtPointer)"ModalHelp");

	/*** context ***/
    widget = XtVaCreateManagedWidget("Context", xmPushButtonWidgetClass,
        helpMenu, NULL);
    XtAddCallback(widget, XmNactivateCallback, PBAct, (XtPointer)"ContextHelp");

    /* END HELP MENU */

    XtManageChild(menuBar);

    return menuBar;
}

