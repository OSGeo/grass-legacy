#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Shell.h>

Widget fo, labe, shel, ne, choos, ma, vect, q, clos, thd;
extern Widget top_level;


extern void new_window();
extern void quit();
extern void destroy_window();
extern void choose_window();
extern void draw_map();
extern void draw_vectors();
extern void show_3D();


static XtCallbackRec draw_map_callback[] = {
		{draw_map, NULL},
		{NULL, NULL}
		};

static XtCallbackRec draw_vectors_callback[] = {
		{draw_vectors, NULL},
		{NULL, NULL}
		};

static XtCallbackRec destroy_win_callback[] = {
		{destroy_window, NULL},
		{NULL, NULL}
		};

static XtCallbackRec new_win_callback[] = {
		{new_window, NULL},
		{NULL, NULL}
		};

static XtCallbackRec quit_callback[] = {
		{quit, NULL},
		{NULL, NULL}
		};	

static XtCallbackRec choose_win_callback[] = {
		{choose_window, NULL},
		{NULL, NULL}
		};

static XtCallbackRec show_3D_callback[] = {
		{show_3D, NULL},
		{NULL, NULL}
		};


void create_main_menu()
{
	Arg arglist[10];
	Cardinal i = 0;


	XtSetArg(arglist[i], XtNwidth, 200); i++;

	fo = XtCreateManagedWidget("form",
			formWidgetClass, top_level,
			arglist, i);


	XtSetArg(arglist[i], XtNheight, 30); i++;
	XtSetArg(arglist[i], XtNlabel, "GRASS GRAPHICS");
	i++;
	labe = XtCreateManagedWidget("label",
			labelWidgetClass, fo,
			arglist, i);
			
	XtSetArg(arglist[2], XtNlabel, "New Window"); 
	XtSetArg(arglist[i], XtNfromVert, labe); i++;
	XtSetArg(arglist[i], XtNcallback, 
		new_win_callback); i++;
	ne =  XtCreateManagedWidget("new",
			commandWidgetClass, fo,
			arglist, i);
			
	XtSetArg(arglist[2], XtNlabel, "Choose window");
	XtSetArg(arglist[3], XtNfromVert, ne ); 	
	XtSetArg(arglist[4], XtNcallback,
				choose_win_callback);
	choos = XtCreateManagedWidget("choose",
			commandWidgetClass, fo,
			arglist, 5 );

	XtSetArg(arglist[2], XtNlabel, "Destroy window");
	XtSetArg(arglist[3], XtNfromVert, choos );
	XtSetArg(arglist[4], XtNcallback,
			destroy_win_callback);
	clos = XtCreateManagedWidget("close",
			commandWidgetClass, fo,
			arglist, 5);

	XtSetArg(arglist[2], XtNlabel, "Draw Map");
	XtSetArg(arglist[3], XtNfromVert, clos );
	XtSetArg(arglist[4], XtNcallback,
				draw_map_callback);
	ma = XtCreateManagedWidget("map",
			commandWidgetClass, fo,
			arglist, 5);

	XtSetArg(arglist[2], XtNlabel, "Draw Vectors");
	XtSetArg(arglist[3], XtNfromVert, ma );
	XtSetArg(arglist[4], XtNcallback,
			draw_vectors_callback);

	vect = XtCreateManagedWidget("vect",
			commandWidgetClass, fo,
			arglist, 5);

	XtSetArg(arglist[2], XtNlabel, "Show 3D View");
	XtSetArg(arglist[3], XtNfromVert, vect );
	XtSetArg(arglist[4], XtNcallback,
				show_3D_callback);

	thd  = XtCreateManagedWidget("3D",
			commandWidgetClass, fo,
			arglist, 5);
			

	XtSetArg(arglist[2], XtNlabel, "Quit");
	XtSetArg(arglist[3], XtNfromVert, thd);
	XtSetArg(arglist[4], XtNcallback,
				quit_callback);
	q = XtCreateManagedWidget("quit",
			commandWidgetClass, fo,
			arglist, 5);
}
