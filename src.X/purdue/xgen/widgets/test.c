#include <stdio.h>

#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/PushB.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include "Table.h"
#include "TableP.h"

static char *sheadings[8] = {
	"X Position",
	"Y Position",
	"Width",
	"Height",
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
	Widget button1;
	Widget button2;
	Widget button3;
	Widget button4;
	Widget button5;
	void CB1();
	void CB2();
	void CB3();
	void CB4();
	void CB5();
	Arg args[30];
	XmString xmLabel;
	XmString headings[8];
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

	for ( i = 0; i < 8; i++ )
		headings[i] = (XmString)XmStringCreateLtoR(sheadings[i],
												   XmSTRING_DEFAULT_CHARSET);

	for ( i = 0; i < 8; i++ )
		Rheadings[i] = (XmString)XmStringCreateLtoR(rheadings[i],
												   XmSTRING_DEFAULT_CHARSET);

	fontlist = XmFontListCreate(XLoadQueryFont(XtDisplay(toplevel),"9x15"),
												   XmSTRING_DEFAULT_CHARSET);
	n = 0;
	XtSetArg(args[n],XmNcolumns,8); n++;
	XtSetArg(args[n],XmNcolumnsDisplayed,4); n++;
	XtSetArg(args[n],XmNcolumnHeadings,headings); n++;
	XtSetArg(args[n],XmNseparator,':'); n++;
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
	button1 = XmCreatePushButton(rc,"button1",args,n);
	XtManageChild(button1);
	XtAddCallback(button1, XmNactivateCallback, CB1, table);
	XmStringFree(xmLabel);

	n = 0;
	XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
	XtSetArg(args[n],XmNfontList,fontlist); n++;
	xmLabel = XmStringCreateLtoR("get column",XmSTRING_DEFAULT_CHARSET);
	XtSetArg(args[n],XmNlabelString,xmLabel); n++;
	button2 = XmCreatePushButton(rc,"button2",args,n);
	XtManageChild(button2);
	XtAddCallback(button2, XmNactivateCallback, CB2, table);
	XmStringFree(xmLabel);

	n = 0;
	XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
	XtSetArg(args[n],XmNfontList,fontlist); n++;
	xmLabel = XmStringCreateLtoR("rowheight = 150",XmSTRING_DEFAULT_CHARSET);
	XtSetArg(args[n],XmNlabelString,xmLabel); n++;
	button3 = XmCreatePushButton(rc,"button3",args,n);
	XtManageChild(button3);
	XtAddCallback(button3, XmNactivateCallback, CB3, table);
	XmStringFree(xmLabel);

	n = 0;
	XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
	XtSetArg(args[n],XmNfontList,fontlist); n++;
	xmLabel = XmStringCreateLtoR("colsDisplayed = 2",XmSTRING_DEFAULT_CHARSET);
	XtSetArg(args[n],XmNlabelString,xmLabel); n++;
	button4 = XmCreatePushButton(rc,"button4",args,n);
	XtManageChild(button4);
	XtAddCallback(button4, XmNactivateCallback, CB4, table);
	XmStringFree(xmLabel);

	n = 0;
	XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;
	XtSetArg(args[n],XmNfontList,fontlist); n++;
	xmLabel = XmStringCreateLtoR("rowsDisplayed = 4",XmSTRING_DEFAULT_CHARSET);
	XtSetArg(args[n],XmNlabelString,xmLabel); n++;
	button5 = XmCreatePushButton(rc,"button5",args,n);
	XtManageChild(button5);
	XtAddCallback(button5, XmNactivateCallback, CB5, table);
	XmStringFree(xmLabel);

	XtRealizeWidget(toplevel);
	while ( 1 ) {
		XEvent ev;

		XtNextEvent(&ev);
		XtDispatchEvent(&ev);
	}
}

void 
CB1(w,cl,ca)
	Widget w;
    caddr_t cl, ca;
{
    exit(0);
}

void 
CB2(w,cl,ca)
	Widget w;
    caddr_t cl, ca;
{
	Widget tw = (Widget) cl;
	XmTableWidget ttw = (XmTableWidget) tw;
	int n = 0;
	Arg arg;

	printf("%s\n",XmTableGetColumn(ttw,1));
}

void 
CB3(w,cl,ca)
	Widget w;
    caddr_t cl, ca;
{
	Widget tw = (Widget) cl;
	XmTableWidget ttw = (XmTableWidget) tw;
	int n = 0;
	Arg arg;

	XtSetArg(arg,XmNrowHeight, 150); n++;
	XtSetValues(tw,arg,n);
}

void 
CB4(w,cl,ca)
	Widget w;
    caddr_t cl, ca;
{
	Widget tw = (Widget) cl;
	XmTableWidget ttw = (XmTableWidget) tw;
	int n = 0;
	Arg arg;

	XtSetArg(arg,XmNcolumnsDisplayed, 2); n++;
	XtSetValues(tw,arg,n);
}

void 
CB5(w,cl,ca)
	Widget w;
    caddr_t cl, ca;
{
	Widget tw = (Widget) cl;
	XmTableWidget ttw = (XmTableWidget) tw;
	int n = 0;
	Arg arg;

	XtSetArg(arg,XmNrowsDisplayed, 4); n++;
	XtSetValues(tw,arg,n);
}
