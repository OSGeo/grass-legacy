#include <stdio.h>

#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/PushB.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include "Table.h"

static char *sheadings[4] = {
	"X Position",
	"Y Position",
	"Width",
	"Height"
};
static char *rheadings[8] = {
	"Box 1",
	"Box 2",
	"Box 3",
	"Box 4",
	"Box 5",
	"Box 6",
	"Box 7",
	"Box 8"
};

main(argc,argv)
int argc;
char **argv;
{
	Display *dpy;
	int scr;
	Widget toplevel;
	Widget rc;
	Widget frame;
	Widget frame2;
	Widget table;
	Widget table2;
	Widget button;
	void CB();
	Arg args[30];
	XmString xmLabel;
	XmString headings[4];
	XmString Rheadings[8];
	XmFontList fontlist;
	Pixel black, white;
	int i;
	int n;

	XtToolkitInitialize();
	dpy = XtOpenDisplay(NULL,NULL,argv[0],argv[0],NULL,0,&argc,argv);
	if ( !dpy ) {
		fprintf(stderr,"Cannot open display\n");
		exit(-1);
	}
	scr = DefaultScreen(dpy);

	n = 0;
	toplevel = 
		XtAppCreateShell(argv[0],NULL,applicationShellWidgetClass,dpy,args,n);

	n = 0;
	XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
	rc = XmCreateRowColumn(toplevel,"RC",args,n);
	XtManageChild(rc);

	n = 0;
	frame = XmCreateFrame(rc,"frame",args,n);
	XtManageChild(frame);

	for ( i = 0; i < 4; i++ )
		headings[i] = (XmString)XmStringCreateLtoR(sheadings[i],
												   XmSTRING_DEFAULT_CHARSET);

	for ( i = 0; i < 8; i++ )
		Rheadings[i] = (XmString)XmStringCreateLtoR(rheadings[i],
												   XmSTRING_DEFAULT_CHARSET);

	fontlist = XmFontListCreate(XLoadQueryFont(XtDisplay(toplevel),"9x15"),
												   XmSTRING_DEFAULT_CHARSET);
	n = 0;
	XtSetArg(args[n],XmNcolumns,4); n++;
	XtSetArg(args[n],XmNcolumnsDisplayed,3); n++;
	XtSetArg(args[n],XmNcolumnHeadings,headings); n++;
	XtSetArg(args[n],XmNrowHeadings,Rheadings); n++;
	XtSetArg(args[n],XmNrows,8); n++;
	XtSetArg(args[n],XmNrowsDisplayed,5); n++;
	XtSetArg(args[n],XmNentryFontList,fontlist); n++;
	xmLabel = XmStringCreateLtoR("titlestring",XmSTRING_DEFAULT_CHARSET);
	XtSetArg(args[n],XmNtitleString,xmLabel); n++;
	table = XmCreateTable(frame,"table",args,n);
	XtManageChild(table);
	XmStringFree(xmLabel);

	for ( i = 0; i < 4; i++ )
		XmStringFree(headings[i]);

	for ( i = 0; i < 8; i++ )
		XmStringFree(Rheadings[i]);

	n = 0;
	XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
	XtSetArg(args[n],XmNfontList,fontlist); n++;
	xmLabel = XmStringCreateLtoR("Exit",XmSTRING_DEFAULT_CHARSET);
	XtSetArg(args[n],XmNlabelString,xmLabel); n++;
	button = XmCreatePushButton(rc,"button",args,n);
	XtManageChild(button);
	XtAddCallback(button, XmNactivateCallback, CB, table);
	XmStringFree(xmLabel);

	XtRealizeWidget(toplevel);
	while ( 1 ) {
		XEvent ev;

		XtNextEvent(&ev);
		XtDispatchEvent(&ev);
	}
}
void 
CB(w,cl,ca)
	Widget w;
    caddr_t cl, ca;
{
    exit(0);
}
