/***********************************************************************

File     	:	cell_editor.c

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	12 January 1990
Last Revised	:
Abstract 	:	This file is contains the main function
			of the cell editor
Returns  	:	None.

***********************************************************************/
/* external declarations, and more includes */
#include "cell_editor_defs.h"
/* X11 headers */
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include "CellEdit.h" /* get the public functions */

static char *fallback_rec[] = {
"Celleditor*quit.translations: #override \\n <Btn1Up>: ExitEditor() reset()",
"Celleditor*value.translations: #override \\n <Key>Return: DoneProc() \n\
					      Ctrl<Key>m:  DoneProc() \n\
					      Ctrl<Key>J:  DoneProc() \n\
					      Ctrl<Key>O:  DoneProc() \n\
					      Ctrl<Key>N:  DoneProc() \n\
					      Meta<Key>V:  DoneProc()",
"Celleditor*quit.label: Quit",
"Celleditor*editor.width: 400",
"Celleditor*editor.height: 400",
NULL,
};

static XtActionsRec editorActions[]=
    {
    {"PopDown",  PopDown },	/* Popdown the load list */
    {"DoneProc", DoneProc},	/* save as return */
    {"ExitEditor", ExitEditor},	/* Exit */
    };

static Widget       	top;       	/* Top level shell */
static Display 		*dpy;
static XtAppContext 	app_con; 
static Boolean 		exitFlag = False;

main(argc, argv)
    int argc;
    char **argv;
    {
    Widget	view;		/* A viewport */
    Widget	loadBtn;	/* The load cell file button */
    Widget	quit;		/* The quit button */
    Widget	saveBtn;	/* save menu button  */
    Widget	form;
    Window	root;
    char progName[80];


    /* we call G_gisinit so all our children get the right prog name */
    /* since X wants this whole vector, we'll copy it for G_gisinit */
    strcpy(progName, argv[0]);
    G_gisinit(progName);

    /* create top level shell */
    top = XtVaAppInitialize(&app_con, "Celleditor", NULL, 0, &argc, argv,
			  fallback_rec, XtNallowShellResize, (XtArgVal)TRUE,
			  NULL);

    /* Add our actions */
    XtAppAddActions(app_con, editorActions, XtNumber(editorActions));

    /* get the display */
    dpy = XtDisplay(top);

    /* create outer widgets */
    form = (Widget)CreateForm(top, "topForm");

    /* create a quit button */
    quit = 
	(Widget)CreateButton(form, "quit");

    /* The load button is a menu button, that pops up a list box
     * of possible files to load
     */
    loadBtn = 
	(Widget)CreateMenuButton(form, "load", "loadShell");

    saveBtn = 
	(Widget)CreateMenuButton(form, "save", "saveShell");

    view = (Widget)CreateViewport(form, "view");

    editor = (Widget)CreateEditor(view);

    /* realize this beast */ 
    XtRealizeWidget(top);
    XtMapWidget(top);
    XFlush(dpy);

    /* Widgets are now initialized */

    /* 
     * create these after the realize 
     * so the the "parents" have windows
     * to query.
     */

    /* Load Cell File pull down menu */
    (void)BuildLoadMenu(loadBtn);
    (void)BuildSaveMenu(saveBtn);

    /* external variable */
    busy = (Window)CreateBusyWindow(form);

    while (1)
	{
	XEvent event;
	XtAppNextEvent(app_con, &event);
	XtDispatchEvent(&event);
	if (exitFlag)
	    {
	    XFlush(dpy);
	    break;
	    }
	}
    XtDestroyApplicationContext(app_con);
    exit(0);
    }

void ExitEditor(w, call, client)
    Widget w;
    XtPointer client;
    XtPointer call;
    {
    char *name;
    XtUnmapWidget(top);
    XtDestroyWidget(top);
    XFlush(dpy);
    exitFlag = True;
    }
