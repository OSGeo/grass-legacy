/***************************************************************
 * This routine takes a commandboard description and creates it.
 **************************************************************/

#include "xgen.h"
#include <X11/StringDefs.h>

void
CreateCommandBoard(s) 
	Shell *s;
{
	Resource *resource;		/* pointer to the current resource 				*/
	Widget 	shell;			/* popup shell widget (parent of all the below) */
	Widget main_window;		/* the MainWindow widget 						*/
	Widget menu_bar;		/* the Menubar widget 							*/
	Widget help;			/* the Help widget 								*/
	Widget frame;			/* the Frame widget 							*/
	Widget bboard;			/* the Bulletin Board widget					*/
	Boolean width_specd, 	/* has the width been specified ? 				*/
			height_specd;	/* has the height been specified ? 				*/
	int n;					/* the Arg counter 								*/
	int popupWidth,
		popupHeight;

	n = 0;
	SetGlobalArgs(&n,NOFONTS);
	SetShellColorArgs(s,&n);
	if ( (resource = IndexResource(s,SHELL,"dx")) != NULL ) {
		int x;

		if ( resource->variable ) ExpandVariable(resource);
		x = (int)((resource->val.dval/100.0) * 
			(double)DisplayWidth(xgenGD.display,xgenGD.screen));
		XtSetArg(args[n],XmNx,x); n++;
	}
	if ( (resource = IndexResource(s,SHELL,"dy")) != NULL ) {
		int y;

		if ( resource->variable ) ExpandVariable(resource);
		y = (int)((resource->val.dval/100.0) *
			(double)DisplayHeight(xgenGD.display,xgenGD.screen));
		XtSetArg(args[n],XmNy,y); n++;
	}
	if ( (resource = IndexResource(s,SHELL,"width")) != NULL ) {
		if ( resource->variable ) ExpandVariable(resource);
		XtSetArg(args[n],XmNwidth,resource->val.ival); n++;
		width_specd = True;
	}
	if ( (resource = IndexResource(s,SHELL,"height")) != NULL ) {
		if ( resource->variable ) ExpandVariable(resource);
		XtSetArg(args[n],XmNheight,resource->val.ival); n++;
		height_specd = True;
	}
	if ( (resource = IndexResource(s,SHELL,"override")) != NULL ) {
		XtSetArg(args[n],XmNoverrideRedirect,resource->val.ival); n++;
	}
	XtSetArg(args[n],XmNallowShellResize,True); n++;
	if ( (resource = IndexResource(s,SHELL,"titlestring")) != NULL ) 
	    s->widget = XtCreatePopupShell(resource->val.cval,
			topLevelShellWidgetClass, xgenGD.applShell,args,n);
	else
	    s->widget = XtCreatePopupShell(s->name,
			topLevelShellWidgetClass, xgenGD.applShell,args,n);

	/***************************************************************
	 * create the MainWindow : this is used since it allows for a
	 * MenuBar, a Frame, and a ScrolledWindow (not used for messageboards).
	 * The application designer might choose to place labels, messages,
	 * or pushbuttons in a messageboard.
	 **************************************************************/
	n = 0;
	SetGlobalArgs(&n,NOFONTS);
	SetShellColorArgs(s,&n);
	main_window = XmCreateMainWindow(s->widget,s->name,args,n);
	XtManageChild(main_window);

	/***************************************************************
	 * create the MenuBar with help button (if help is supplied)
	 **************************************************************/
	menu_bar = NULL;
	if ( (resource = IndexResource(s,SHELL,"help")) != NULL ) {
		n = 0;
		SetGlobalArgs(&n,NOFONTS);
		SetShellColorArgs(s,&n);
		menu_bar = XmCreateMenuBar(main_window,"menu_bar",args,n);
		XtManageChild(menu_bar);
		n = 0;
		SetGlobalArgs(&n,FONTS);
		SetShellColorArgs(s,&n);
		help = XmCreateCascadeButton(menu_bar, "Help", args,n);
		XtManageChild(help);
		/***************************************************************
	 	 * add the help callback, passing it the help string
	 	 **************************************************************/
		XtAddCallback(help,XmNactivateCallback,helpCB,
						(caddr_t)resource->val.cval);

		n = 0;
		XtSetArg(args[n],XmNmenuHelpWidget,help); n++;
		XtSetValues(menu_bar,args,n);
	}

	/***************************************************************
	 * create a frame for the objects 
	 **************************************************************/
	n = 0;
	SetGlobalArgs(&n,NOFONTS);
	SetShellColorArgs(s,&n);
	frame = XmCreateFrame(main_window,"frame",args,n);
	XtManageChild(frame);

	/***************************************************************
	 * create a bulletin board widget to manage the objects 
	 **************************************************************/
	n = 0;
	SetGlobalArgs(&n,NOFONTS);
	SetShellColorArgs(s,&n);
	XtSetArg(args[n], XmNpacking, XmPACK_COLUMN); n++;
	XtSetArg(args[n], XmNnumColumns, 1); n++;
	bboard = XmCreateBulletinBoard(frame,"bb",args,n);
	XtManageChild(bboard);

    /***************************************************************
	 * set main window areas 
	 **************************************************************/
	XmMainWindowSetAreas(main_window,menu_bar,NULL,NULL,NULL,frame);
	XtRealizeWidget(s->widget);

	/***************************************************************
	 * add bboard to tab group 
	 * (enable <TAB> and arrow keys focus mechanism) 
	 **************************************************************/
	XmAddTabGroup(bboard);

	/***************************************************************
	 * if the shell has no objects....
	 **************************************************************/
	if ((Widget)0 == CreateObject(s,bboard,False)) {
		printf("Oops, no objects.....\n");
	}

	n = 0;
	XtSetArg(args[n],XtNwidth,&popupWidth); n++;
	XtSetArg(args[n],XtNheight,&popupHeight); n++;
	XtGetValues(s->widget,args,n);

	n = 0;
    if ( (resource = IndexResource(s,SHELL,"x")) != NULL ) {
		if ( resource->variable ) ExpandVariable(resource);
        if ( resource->val.ival < 0 ) {
            XtSetArg(args[n],XmNx,DisplayWidth(xgenGD.display,xgenGD.screen)
                - popupWidth + resource->val.ival); n++;
        } else {
            XtSetArg(args[n],XmNx,resource->val.ival); n++;
        }
    }
    if ( (resource = IndexResource(s,SHELL,"y")) != NULL ) {
		if ( resource->variable ) ExpandVariable(resource);
        if ( resource->val.ival < 0 ) {
            XtSetArg(args[n],XmNx,DisplayWidth(xgenGD.display,xgenGD.screen)
                - popupHeight + resource->val.ival); n++;
        } else {
            XtSetArg(args[n],XmNy,resource->val.ival); n++;
        }
    }
    XtSetValues(s->widget,args,n);
}

