/* Callback of command button "New Window" of main menu	*/

#ifndef         INCLUDE
#define         INCLUDE

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#endif

#include <X11/Xutil.h>
#include <X11/Shell.h>

#define	nActions(x) 	(sizeof(x)/sizeof(XtActionsRec))

int no_disp_wins = 0;

Window active_window = NULL;
Widget display_win[50], children[50];
Pixmap pixmap = 0;
XSetWindowAttributes attributes;

void click(widget, event, params, nparams)
	Widget widget;
	XEvent *event;
	char **params;
	int *nparams;
{
	extern int destroy_flag, choose_flag;

	if(destroy_flag == 1)
	{
	destroy_flag = 0;
	XtUnmapWidget(XtParent(widget));
	}

	if(choose_flag == 1)
	{
	active_window = XtWindow(widget);
	printf("\n active window id = %u", active_window);
	choose_flag = 0;
	}
}



void show(widget, event, params, nparams)
	Widget widget;
	XEvent *event;
	char **params;
	int *nparams;
{
	Window the_window;
	Display *the_display;
	Drawable drawable_root;
	int x, y, width, height, border_width,
		drawable_depth;
	GC the_GC;
	XGCValues the_GC_values;
	unsigned long the_GC_valuemask;

	

	printf("\n Expose");


	the_window = XtWindow(widget);
	the_display= XtDisplay(widget);

	XGetWindowAttributes(the_display,
			the_window,
			&attributes);

	if(attributes.background_pixmap != None)
	{
		printf("\n None");
	pixmap = attributes.background_pixmap;


	the_GC = XCreateGC(the_display, the_window,
			the_GC_valuemask,
			&the_GC_values);

	XGetGeometry(the_display,the_window,
		&drawable_root,&x,&y,&width,
		&height,&border_width,
		&drawable_depth);


	XCopyArea(the_display, pixmap, the_window,
		the_GC, 0, 0, width, height, 0, 0);
	XFlush(the_display);
	}
}



static XtActionsRec win_acts_tbl[] = {
	{"click", click},
	{"show", show},
};

static char *click_trans_tbl = 
	"<BtnDown>:click()\n\
	<Expose>:show()";

extern Arg win_args[10];
extern Arg shell_args[10];

XtTranslations click_trans;

void new_window(new, client_data, call_data)
	Widget new;
	caddr_t client_data;
	caddr_t call_data;
{
	char c, d;
	char ns[5], nw[5];
	Arg arglist[10];
	Window n_win;
	XSetWindowAttributes attributes;

	c = 'a' + no_disp_wins; 
	d = 'A' + no_disp_wins;

	sprintf(ns, "%c", c);
	sprintf(nw, "%c", d);

	display_win[no_disp_wins] = 
			XtCreateApplicationShell(ns,
			topLevelShellWidgetClass,
			shell_args, 10);

	children[no_disp_wins] = 
			XtCreateManagedWidget(nw,
			widgetClass,
			display_win[no_disp_wins],
			win_args, 3);

	/* if(no_disp_wins == 0)
	{
	*/
	XtAddActions(win_acts_tbl, nActions(win_acts_tbl));

	click_trans= XtParseTranslationTable(click_trans_tbl);
	/*
	}
	*/
	XtOverrideTranslations(children[no_disp_wins],
				click_trans);
	
	XtRealizeWidget(display_win[no_disp_wins]);
	
	n_win = XtWindow(display_win[no_disp_wins]);

	attributes.backing_store = Always;
	attributes.save_under = True;
	XChangeWindowAttributes(XtDisplay(new),
		n_win, CWBackingStore | CWSaveUnder,
		&attributes);


	no_disp_wins++;
}
