#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/AsciiText.h>
#include <X11/Box.h>
#include <X11/Form.h>
#include <X11/Command.h>
#include <X11/Label.h>
#include <X11/Shell.h>
#include "icon"
#define	nActions(x)	(sizeof(x)/sizeof(XtActionsRec))

extern Display *the_display;
extern int the_screen;
extern Widget top_level;
Widget fo, display, cell, gisenv,
	vector, threed, erase, 
	qt, title, seq,
	outline, wind, coors;
Pixmap disp_icon;

extern void quit();
extern void list_cells();
extern void list_vects();
extern void erase_display();
extern void show_3D();
extern void replay();
extern void run_shell();
extern void change_window();
extern void change_enter();
extern void change_leave();
extern void change_environment();
extern void display_coors();

static XtCallbackRec shell_callback[] = {
		{run_shell, NULL},
		{NULL, NULL}
		};


static XtCallbackRec quit_callback[] = {
		{quit, NULL},
		{NULL, NULL}
		};

static XtCallbackRec list_cells_callback[] = {
		{list_cells, NULL},
		{NULL, NULL}
		};

static XtCallbackRec list_vects_callback[] = {
		{list_vects, NULL},
		{NULL, NULL}
		};




static XtCallbackRec change_window_callback[] = {
		{change_window, NULL},
		{NULL, NULL}
		};

static XtCallbackRec change_environment_callback[] = {
		{change_environment, NULL},
		{NULL, NULL}
		};


static XtCallbackRec erase_callback[] = {
		{erase_display, NULL},
		{NULL, NULL}
		};

static XtCallbackRec show_3D_callback[] = {
		{show_3D, NULL},
		{NULL, NULL}
		};

static XtActionsRec win_acts_tbl[] = {
	{"replay", replay},
	{"display_coors", display_coors},
};

static XtActionsRec form_acts_tbl[] = {
	{"change_enter", change_enter},
	{"change_leave", change_leave},
};

static char *form_trans_tbl =
	"<Unmap>: change_enter()\n\
	 <EnterWindow>: change_enter()\n\
	 <LeaveWindow>: change_leave()";



static char *win_trans_tbl =
	"<Configure>:replay()\n\
	 <Motion>:display_coors()";	


XtTranslations win_trans, form_trans;

void create_display()
{
	Arg arglist[15];
	int pixel();
	extern XFontStruct *font1, *font2, *font5;
	int  disp_w, disp_h;
	extern Cursor cross_hair;

	create_fonts();
	create_cursors();


	disp_icon = XCreatePixmapFromBitmapData(the_display,
	DefaultRootWindow(the_display), icon_bits, 64, 58,
	 (unsigned long) pixel("red"), 
	 (unsigned long) pixel("yellow"), 1);


	disp_w = XDisplayWidth(the_display, the_screen);
	disp_h = XDisplayHeight(the_display, the_screen);
	
	
	
	
	XtSetArg(arglist[0], XtNminWidth, 600);
	XtSetArg(arglist[1], XtNminHeight, 500);
	XtSetArg(arglist[2], XtNheight, 500);
	XtSetArg(arglist[3], XtNwidth,  600);
	XtSetArg(arglist[4], XtNmaxWidth, disp_w);
	XtSetArg(arglist[5], XtNmaxHeight, disp_h);
	XtSetArg(arglist[6], XtNiconPixmap, disp_icon);

	outline = XtCreateApplicationShell("outline",
		topLevelShellWidgetClass,
		arglist, 7);





	XtSetArg(arglist[0], XtNheight, 500);
	XtSetArg(arglist[1], XtNwidth,  600); 
	XtSetArg(arglist[2], XtNborderColor, 
				pixel("white"));
	XtSetArg(arglist[3], XtNbackground,
				pixel("white"));


	fo = XtCreateManagedWidget("form_big",
			formWidgetClass, outline,
			arglist, 4);

	XtAddActions(form_acts_tbl, nActions(form_acts_tbl));
	form_trans= XtParseTranslationTable(form_trans_tbl);
	XtOverrideTranslations(fo, form_trans);
	
	
	
	XtSetArg(arglist[0], XtNheight, 20);
	XtSetArg(arglist[1], XtNlabel, "GRASS  DISPLAY  WINDOW");
	XtSetArg(arglist[2], XtNleft, XtChainLeft);
	XtSetArg(arglist[3], XtNright, XtChainRight);
	XtSetArg(arglist[4], XtNtop, XtChainTop);
	XtSetArg(arglist[5], XtNbottom, XtChainTop);
	XtSetArg(arglist[6], XtNwidth, 600); 
	XtSetArg(arglist[7], XtNfont, font1);
	XtSetArg(arglist[8], XtNbackground,
				pixel("red"));
	XtSetArg(arglist[9], XtNforeground,
				pixel("yellow"));
	XtSetArg(arglist[10], XtNborderColor,
				pixel("black"));


	title = XtCreateManagedWidget("title",
			labelWidgetClass, fo,
			arglist, 11);


	XtSetArg(arglist[0], XtNheight, 20);
	XtSetArg(arglist[1], XtNwidth,  65);
	XtSetArg(arglist[2], XtNleft, XtChainLeft);
	XtSetArg(arglist[3], XtNright, XtChainLeft);
	XtSetArg(arglist[4], XtNtop, XtChainTop);
	XtSetArg(arglist[5], XtNbottom, XtChainTop);	
	XtSetArg(arglist[6], XtNfromVert, title);
	XtSetArg(arglist[7], XtNcallback, change_environment_callback);
	XtSetArg(arglist[8], XtNfont, font2);
	XtSetArg(arglist[9], XtNbackground,
				pixel("blue"));
	XtSetArg(arglist[10], XtNforeground,
				pixel("yellow"));
	XtSetArg(arglist[11], XtNborderColor,
				pixel("black"));
	XtSetArg(arglist[12], XtNhighlightThickness, 11);

	gisenv = XtCreateManagedWidget("GISEN",
			commandWidgetClass, fo,
			arglist, 13);


	XtSetArg(arglist[7], XtNcallback, change_window_callback);	
	XtSetArg(arglist[13], XtNfromHoriz, gisenv);	
	wind = XtCreateManagedWidget("WIND",
			commandWidgetClass, fo,
			arglist, 14);



	XtSetArg(arglist[7], XtNcallback, list_cells_callback);
	XtSetArg(arglist[13], XtNfromHoriz, wind); 

	
	cell = XtCreateManagedWidget("CELL",
			commandWidgetClass, fo,
			arglist, 14);

	XtSetArg(arglist[7], XtNcallback, list_vects_callback);
	XtSetArg(arglist[13], XtNfromHoriz, cell);
	vector = XtCreateManagedWidget("VECT",
			commandWidgetClass, fo,
			arglist, 14);
	
	XtSetArg(arglist[7], XtNcallback, show_3D_callback);
	XtSetArg(arglist[13], XtNfromHoriz, vector);
	threed = XtCreateManagedWidget("3D",
			commandWidgetClass, fo,
			arglist, 14);
	
	XtSetArg(arglist[7], XtNcallback, shell_callback);
	XtSetArg(arglist[13], XtNfromHoriz, threed);
	seq = XtCreateManagedWidget("SHELL",
			commandWidgetClass, fo,
			arglist, 14);

	XtSetArg(arglist[7], XtNcallback, erase_callback);
	XtSetArg(arglist[13], XtNfromHoriz, seq);	
	erase = XtCreateManagedWidget("ERASE",
			commandWidgetClass, fo,
			arglist, 14);

	XtSetArg(arglist[7], XtNcallback, quit_callback);
	XtSetArg(arglist[13], XtNfromHoriz, erase);
	qt = XtCreateManagedWidget("EXIT",
			commandWidgetClass, fo,
			arglist, 14);
	

	XtSetArg(arglist[0], XtNheight, 16);
	XtSetArg(arglist[1], XtNwidth, 112); 
	XtSetArg(arglist[2], XtNleft, XtChainRight);
	XtSetArg(arglist[3], XtNright, XtChainRight);
	XtSetArg(arglist[4], XtNtop, XtChainBottom);
	XtSetArg(arglist[5], XtNbottom, XtChainBottom);
	XtSetArg(arglist[6], XtNfromVert, title);
	XtSetArg(arglist[7], XtNvertDistance, 414);
	XtSetArg(arglist[8], XtNbackground,
				pixel("white"));
	XtSetArg(arglist[9], XtNforeground,
				 pixel("black"));
	XtSetArg(arglist[10], XtNborderColor,
				 pixel("black"));
	XtSetArg(arglist[11], XtNfromHoriz, gisenv);
	XtSetArg(arglist[12], XtNhorizDistance, 421);

	
	
	
	
	coors = XtCreateManagedWidget("title",
				 widgetClass, fo,
				 arglist, 13);



	XtSetArg(arglist[0], XtNheight, 400);
	XtSetArg(arglist[1], XtNwidth, 600);
	XtSetArg(arglist[2], XtNfromVert,  cell); 
	XtSetArg(arglist[3], XtNtop, XtChainTop);
	XtSetArg(arglist[4], XtNbottom, XtChainBottom);
	XtSetArg(arglist[5], XtNright, XtChainRight);
	XtSetArg(arglist[6], XtNleft, XtChainLeft);
	XtSetArg(arglist[7], XtNbackground,
			pixel("thistle"));
	XtSetArg(arglist[8], XtNcursor, cross_hair);

	display = XtCreateManagedWidget("",
			labelWidgetClass, fo,
			arglist, 9);

	XtAddActions(win_acts_tbl, nActions(win_acts_tbl));
	win_trans= XtParseTranslationTable(win_trans_tbl);
	XtOverrideTranslations(display, win_trans);















}
