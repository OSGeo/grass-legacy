
#define XZOOMMAIN
#define X11STUFF
 
#include <stdio.h>

/* 
 * Standard Toolkit include files:
 */
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <X11/StringDefs.h>

/*
 * Public include files for widgets used in this file.
 */
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Shell.h>
#include "xzoom.h"
#include "xzoom.icon"
#define REDRAW 2000
static Widget	backgnd, version;
	static int
redraw_cnt = 0;

static Widget pane, cmd_box;
static struct {
	Widget	pen;
	char	name[3];
} lab_pens[MAXPENS]; /* pen color widgets */
	static unsigned int
last_xTablet, last_yTablet;
	unsigned int
get_size(width, height) unsigned int *width, *height; {
	int x, y;
	unsigned border, depth, tmp;
	Window root;

	if (!width)
		width = height = &tmp;
	XGetGeometry(XtDisplay(topLevel),XtWindow(Tablet),&root,
		&x,&y,width,height,&border,&depth);
	return(depth);
}
	void
redrawT(w, event) Widget w;  XExposeEvent *event; {
	int x, y, width, height;

	if (event) {
		x = event->x;
		y = event->y;
		height = event->height;
		width = event->width;
	} else {
		x = y = 0;
		width = xTablet;
		height = yTablet;
	}
	if (DefaultDepthOfScreen(XtScreen(w)) > 1)
		XCopyArea(XtDisplay(w),  picture, XtWindow(w), pcopy,
			x, y, width, height, x, y);
	else
		XCopyPlane(XtDisplay(w),  picture, XtWindow(w), pcopy,
			x, y, width, height, x, y, 1);
	redraw_cnt = 0;
}
	void
clearT() {
	XFillRectangle(XtDisplay(topLevel), picture, pclear,
		0, 0, xTablet, yTablet);
}
	void
plinesT(n, pen, xy) XPoint *xy; { /* plotter lines drafting */
	XDrawLines(XtDisplay(topLevel), picture, pens[pen], xy, n,
		CoordModeOrigin);
	if ((redraw_cnt += n) > REDRAW)
		redrawT(Tablet, (XExposeEvent *)0);
}
	void
lineT(x1, y1, x2, y2) { /* basic line drawting of digitzed data */
	int dx, dy;
	XExposeEvent fake;

	XDrawLine(XtDisplay(topLevel), picture, draw, x1, y1, x2, y2);
	fake.width = abs( dx = x1 - x2 ) + 1;
	fake.height = abs( dy = y1 - y2 ) + 1;
	fake.x = dx > 0 ? x2 : x1;
	fake.y = dy > 0 ? y2 : y1;
	redrawT(Tablet, &fake);
}
	void
circleT(x, y) { /* to draw circle digitizing marks */
	XExposeEvent fake;

	x -= 3;
	y -= 3;
	XDrawArc(XtDisplay(topLevel), picture, draw, x, y, 7, 7, 0, 23040);
	fake.x = x;
	fake.y = y;
	fake.width = fake.height = 8;
	redrawT(Tablet, &fake);
}
/* system error message values */
extern int errno, sys_nerr;
extern char *sys_errlist[];
	static char
*Cline1, *Cline2;
/*ARGSUSED*/
	void
redrawC(w, event) Widget w;  XExposeEvent *event; {
	XClearWindow(XtDisplay(topLevel), XtWindow(Comment));
	if (Cline1)
		XDrawString(XtDisplay(topLevel), XtWindow(Comment), cdraw,
			10, 11, Cline1, strlen(Cline1));
	if (Cline2)
		XDrawString(XtDisplay(topLevel), XtWindow(Comment), cdraw,
			10, 26, Cline2, strlen(Cline2));
	if (*annot_L1)
		XDrawString(XtDisplay(topLevel), XtWindow(Comment), cdraw,
			xTablet/2, 11, annot_L1, strlen(annot_L1));
	if (*annot_L2)
		XDrawString(XtDisplay(topLevel), XtWindow(Comment), cdraw,
			xTablet/2, 26, annot_L2, strlen(annot_L2));
}
	void
docomment(line1, line2, sys) char *line1, *line2; {
	Cline1 = line1;
	Cline2 = line2;
	if (!Cline2 && sys && errno > 0 && errno < sys_nerr)
		Cline2 = sys_errlist[errno];
	(void)redrawC(Comment, (XExposeEvent *)0);
}
/*ARGSUSED*/
	void
but1T(w, event) Widget w;  XButtonEvent *event; {
	if (but_return)
		but_return(1,event->x,event->y);
}
/*ARGSUSED*/
	void
but2T(w, event) Widget w;  XButtonEvent *event; {
	if (but_return)
		but_return(2,0,0);
}
/*ARGSUSED*/
	void
but3T(w, event) Widget w;  XButtonEvent *event; {
	if (but_return)
		but_return(3,0,0);
}
	void
ratLive(w, event) Widget w;  XButtonEvent *event; {
	static int dx, dy;

	switch (rat_mode) {
	case RAT_LINE:
		if (next_rat)
			XDrawLine(XtDisplay(w), XtWindow(w), dr_gc,
				base_rat_x, base_rat_y,
				next_rat_x, next_rat_y);
		else
			next_rat = 1;
		if (!event)
			break;
		next_rat_x = event->x;
		next_rat_y = event->y;
		XDrawLine(XtDisplay(w), XtWindow(w), dr_gc,
			base_rat_x, base_rat_y,
			next_rat_x, next_rat_y);
		break;
	case RAT_BOX:
		if (next_rat)
			XDrawRectangle(XtDisplay(w), XtWindow(w), dr_gc,
				dx < 0 ? next_rat_x : base_rat_x,
				dy < 0 ? next_rat_y : base_rat_y,
				abs(dx),abs(dy));
		else
			next_rat = 1;
		if (!event)
			break;
		next_rat_x = event->x;
		next_rat_y = event->y;
		dx = next_rat_x - base_rat_x;
		dy = next_rat_y - base_rat_y;
		XDrawRectangle(XtDisplay(w), XtWindow(w), dr_gc,
			dx < 0 ? next_rat_x : base_rat_x,
			dy < 0 ? next_rat_y : base_rat_y,
			abs(dx),abs(dy));
		break;
	default:
		break;
	}
}
/*ARGSUSED*/
	void
resizeT(w, event) Widget w; XConfigureEvent *event; {
	(void)get_size(&xTablet, &yTablet);
	if (xTablet != last_xTablet || yTablet != last_yTablet) {
		XFreePixmap( XtDisplay(topLevel), picture);
		picture = XCreatePixmap(XtDisplay(topLevel),
			RootWindowOfScreen(XtScreen(topLevel)),
			xTablet, yTablet, depth);
		last_xTablet = xTablet;
		last_yTablet = yTablet;
	}
	if (Zscaled)
		(void)runplot();
	else { /* for cosmetic purposes only */
		clearT();
		redrawT(Tablet, (XExposeEvent *)0);
	}
}
/*---------------------------------------------------*/
/* Translations for Tablet */
	static
String transT =
	"<Expose>:	redrawT() \n\
	<Configure>:	resizeT() \n\
	<Btn1Down>:	but1T() \n\
	<Btn2Down>:	but2T() \n\
	<Btn3Down>:	but3T() \n\
	<MouseMoved>:	ratLive()";
	static
XtActionsRec actionsT[] = {
	{"redrawT",	redrawT},
	{"resizeT",	resizeT},
	{"but1T",	but1T},
	{"but2T",	but2T},
	{"but3T",	but3T},
	{"ratLive",	ratLive},
};
/* Translations for Comment */
	static
String transC =
	"<Expose>:	redrawC()";
	static
XtActionsRec actionsC[] = {
	{"redrawC",	redrawC},
};
/* Override translations for dialog */
	static String
dialogTrans =
	"#override \n\
	Ctrl<Key>J: no-op(RingBell) \n\
	Ctrl<Key>M: no-op(RingBell) \n\
	Ctrl<Key>N: no-op(RingBell) \n\
	Ctrl<Key>O: no-op(RingBell) \n\
	Ctrl<Key>Z: no-op(RingBell) \n\
	Meta<Key>I: no-op(RingBell) \n\
	Meta<Key>V: no-op(RingBell) \n\
	Meta<Key>Z: no-op(RingBell) \n\
	<Key>Return: no-op(RingBell) \n\
	<Key>Linefeed: no-op(RingBell)";
/*---------------------------------------------------*/
	static void
setup(w) Widget w; {
	XGCValues values;
	Arg arg[2];
	unsigned long fore;
	int i;

		/* graphics setup */
	XtSetArg(arg[0], XtNforeground, &values.foreground);
	XtSetArg(arg[1], XtNbackground, &values.background);
	XtGetValues(backgnd, arg, 2);
	fore = values.foreground;
	values.function = GXxor;
	if (! fore) values.foreground = values.background;
	dr_gc = XCreateGC(XtDisplay(w),  /* drag box/line */
		RootWindowOfScreen(XtScreen(w)),
		GCFunction |GCForeground| GCBackground , &values);
	values.foreground = fore;
	picture = XCreatePixmap(XtDisplay(w),
		RootWindowOfScreen(XtScreen(w)),
		xTablet, yTablet, depth);
	last_xTablet = xTablet;
	last_yTablet = yTablet;
	values.function = GXcopy;
	draw = XCreateGC(XtDisplay(w), picture,
		GCFunction| GCForeground| GCBackground, &values);
	pcopy = XCreateGC(XtDisplay(w),
		RootWindowOfScreen(XtScreen(w)), GCFunction, &values);
	XtSetArg(arg[0], XtNbackground, &values.foreground);
	for (i = 0; i < MAXPENS ; ++i) {
		XtGetValues(lab_pens[i].pen, arg, 1);
		if (values.foreground == values.background)
			values.foreground = fore;
		pens[i] = XCreateGC(XtDisplay(w), picture,
			GCFunction| GCForeground, &values);
	}
	values.foreground = values.background;
	pclear = XCreateGC(XtDisplay(w), picture,
		GCFunction| GCForeground|GCBackground, &values);
	XFillRectangle(XtDisplay(w), picture, pclear,
		0, 0, xTablet, yTablet);

		/* comment field setup */
	values.function = GXcopy;
	values.foreground = fore;
	cdraw = XCreateGC(XtDisplay(w), RootWindowOfScreen(XtScreen(w)),
		GCFunction| GCForeground| GCBackground, &values);
	values.foreground = values.background;
	docomment((char *)0, (char *)0, 0);
}
	static Arg
A_version[] = {
	XtNlabel, (XtArgVal) XVERSION,
},
A_plottext[] = {
	XtNscrollVertical, (XtArgVal)XawtextScrollAlways,
	XtNeditType, (XtArgVal)XawtextEdit,
	XtNtype, (XtArgVal)XawAsciiFile,
	XtNheight, (XtArgVal)80,
},
A_Ltext[] = {
	XtNheight, (XtArgVal)60,
	XtNscrollVertical, (XtArgVal)XawtextScrollAlways,
	XtNeditType, (XtArgVal)XawtextEdit,
	XtNtype, (XtArgVal)XawAsciiString,
	XtNstring, (XtArgVal)"",
};
main(argc, argv) int argc; char **argv; {
	Arg args[5];
	int i;

	topLevel = XtInitialize( argv[0], "XZoom", NULL, 0, &argc, argv);
		/* set icon */
	XtSetArg(args[0], XtNiconPixmap,
		XCreateBitmapFromData(XtDisplay(topLevel),
		RootWindowOfScreen(XtScreen(topLevel)),  xzoom_bits,
		xzoom_width, xzoom_height) );
	XtSetValues(topLevel, args, 1);
		/* make Widgets */
		/* 5 part pane window */
	pane = XtCreateManagedWidget("pane", panedWidgetClass,
		topLevel, NULL, 0);
		/* box for command basic command widgets */
	cmd_box = XtCreateManagedWidget( "cmd_box", boxWidgetClass,
		pane, NULL, 0);
	version = XtCreateManagedWidget("Version",
		labelWidgetClass, cmd_box, A_version, XtNumber(A_version));
		/* setup command widgets */
	for (i = 0; list[i].name ; ++i) { /* button commands */
		list[i].w = XtCreateManagedWidget( list[i].name,
			commandWidgetClass, cmd_box, NULL, 0);
		XtAddCallback(list[i].w, XtNcallback, list[i].c, (caddr_t)0);
	}
		/* file name pop-up */
	popshell = XtCreatePopupShell("File Name",
		transientShellWidgetClass, topLevel, NULL, 0);
	dialog = XtCreateManagedWidget("dialog",
		dialogWidgetClass, popshell, NULL, 0);
	dialogTr = XtParseTranslationTable(dialogTrans);

	dialogdone = XtCreateManagedWidget("ENTER",
		commandWidgetClass, dialog, NULL, 0);
	XtAddCallback(dialogdone, XtNcallback, Cend_dialog, dialog);
	overwrite = XtCreateManagedWidget("OVERWRITE",
		commandWidgetClass, dialog, NULL, 0);
	XtAddCallback(overwrite, XtNcallback, Cend_overwrite, dialog);
	appendit = XtCreateManagedWidget("APPEND",
		commandWidgetClass, dialog, NULL, 0);
	XtAddCallback(appendit, XtNcallback, Cend_append, dialog);
	abortit = XtCreateManagedWidget("ABORT",
		commandWidgetClass, dialog, NULL, 0);
	XtAddCallback(abortit, XtNcallback, Cend_abortit, dialog);
		/* plotter command pop-up */
	popplot = XtCreatePopupShell("Plotter",
		transientShellWidgetClass, topLevel, NULL, 0);
	plotbox = XtCreateManagedWidget("plotbox", boxWidgetClass,
		popplot, NULL, 0);
	plotdon = XtCreateManagedWidget("DONE",
		commandWidgetClass, plotbox, NULL, 0);
	XtAddCallback(plotdon, XtNcallback, CPlotdon, "D");
	plotabt = XtCreateManagedWidget("ABORT",
		commandWidgetClass, plotbox, NULL, 0);
	XtAddCallback(plotabt, XtNcallback, CPlotdon, "A");
		/* plotter file edit command pop up */
        plottext = XtCreateManagedWidget( "text", asciiTextWidgetClass,
		plotbox, A_plottext, XtNumber(A_plottext));
		/* comment window */
	XtSetArg(args[0], XtNtranslations,
		(XtArgVal)XtParseTranslationTable(transC));
	XtSetArg(args[1], XtNheight, (XtArgVal)yComment);
	XtSetArg(args[2], XtNwidth, (XtArgVal)xTablet);
	Comment = XtCreateManagedWidget( "Comment", simpleWidgetClass,
		pane, args, 3);
		/* text entry window */
        Ltext = XtCreateManagedWidget( "Ltext",
                asciiTextWidgetClass, pane, A_Ltext, XtNumber(A_Ltext));
		/* separator between text and graphics */
        textlabel = XtCreateManagedWidget( "textlabel",
                boxWidgetClass, pane, NULL, 0 );
	backgnd = XtCreateManagedWidget("Back",
			labelWidgetClass, textlabel, NULL, 0 );
	XtSetArg(args[0], XtNlabel, (XtArgVal)"B");
	XtSetValues(backgnd, args, 1);
	for (i = 0; i < MAXPENS; ++i) {
		(void)sprintf(lab_pens[i].name, "%d", i);
		lab_pens[i].pen = XtCreateManagedWidget(lab_pens[i].name,
			commandWidgetClass, textlabel, NULL, 0 );
		XtAddCallback(lab_pens[i].pen, XtNcallback, Cpen_out,
			lab_pens[i].name);
	}
	lltext = XtCreateManagedWidget( "labeltext",
		labelWidgetClass, textlabel, NULL, 0 );
	XtSetArg(args[0], XtNlabel, (XtArgVal)"< pen colors, text ^");
	XtSetValues(lltext, args, 1);
		/* graphics window */
	XtSetArg(args[0], XtNtranslations,
		(XtArgVal)XtParseTranslationTable(transT));
	XtSetArg(args[1], XtNwidth, (XtArgVal)xTablet);
	XtSetArg(args[2], XtNheight, (XtArgVal)yTablet);
	Tablet = XtCreateManagedWidget( "Tablet", simpleWidgetClass,
		pane, args, 3);
		/* remainder of non-pane setup */
	XtAddActions(actionsC, XtNumber(actionsC));
	XtAddActions(actionsT, XtNumber(actionsT));
		/* do it */
	XtRealizeWidget(topLevel);
	depth = get_size(&xTablet,&yTablet);
	setup(topLevel);
	sensitivity(1);
	{ /* initialize lltext and Comment color */
		Pixel back, fore;

		XtSetArg(args[0], XtNbackground, (XtArgVal)&back);
		XtSetArg(args[1], XtNforeground, (XtArgVal)&fore);
		XtGetValues(lab_pens[pen_out].pen, args, 2);
		XtSetArg(args[0], XtNbackground, (XtArgVal)back);
		XtSetArg(args[1], XtNforeground, (XtArgVal)fore);
		XtSetValues(lltext, args, 2);

		XtSetArg(args[0], XtNbackground, (XtArgVal)&back);
		XtGetValues(backgnd, args, 1);
		XtSetArg(args[0], XtNbackground, (XtArgVal)back);
		XtSetValues(Comment, args, 1);
	}
	{ /* set approximate screen scaling */
		int j;
		i = DisplayWidth(XtDisplay(topLevel),
			DefaultScreen(XtDisplay(topLevel)));
		j = DisplayWidthMM(XtDisplay(topLevel),
			DefaultScreen(XtDisplay(topLevel)));
		xpix_per_cm = (10. * i)/j;
		i = DisplayHeight(XtDisplay(topLevel),
			DefaultScreen(XtDisplay(topLevel)));
		j = DisplayHeightMM(XtDisplay(topLevel),
			DefaultScreen(XtDisplay(topLevel)));
		ypix_per_cm = (10. * i)/j;
	}
	XtMainLoop();
}
