/***********************************************************************

File     	:	build_save_menu.c
Function 	:	(void)BuildSaveMenu(parent)
Args	 	:	    Widget parent; -- parent menu button

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	15 February 1990
Last Revised	:
Abstract 	:	Build the save menu.
Returns  	:	None.

***********************************************************************/
# include "cell_editor.h"
# include "CellEdit.h"
# include <X11/Xaw/Dialog.h>

static void CancelProc();
static void SaveProc();

/******
XtCallbackRec saveCalls[] =
    {
    {SaveProc,  NULL},
    {NULL,      NULL}
    };
*****/

static Widget saveShell;
static Widget saveBtn;
static Widget saveAsBtn;
static Widget dialogShell;
static Widget dialogBox;

void BuildSaveMenu(parent)
    Widget parent;
    {
    Widget text;

    /* create shell  and sub objects */
    saveShell = (Widget)CreatePopupShell(parent, "saveShell", 1);
    saveBtn = (Widget)CreateBSB(saveShell, "save");
    saveAsBtn = (Widget)CreateBSB(saveShell,"saveAs");
    XtAddCallback(saveBtn, XtNcallback, SaveProc, (XtPointer)NULL);
    XtAddCallback(saveAsBtn, XtNcallback, SaveProc, (XtPointer)NULL);

    /* create a dialog popup to get the file name */
    dialogShell = (Widget)CreatePopupShell(parent, "saveShell", 2);
    dialogBox = (Widget)CreateDialogBox(dialogShell, "fileName");
    if ( (text = XtNameToWidget(dialogBox, "value")) == (Widget)NULL)
	G_fatal_error("Unable to find dialog's text widget\n");
    else
	{
	XtSetKeyboardFocus(dialogBox, text);
	}
    XawDialogAddButton(dialogBox,"Done", DoneProc, NULL);
    XawDialogAddButton(dialogBox,"Cancel", CancelProc, NULL);
    }

/* externally declared action */
static void SaveProc(w, client, call)
    Widget w;
    XtPointer client;
    XtPointer call;
    {
    int x,y;
    int status;

    if (w == saveBtn)
	{
	status = SetSaveFile(editor, NULL);
	if (status == 0)
	    {
	    G_warning("You must first load a file");
	    return;
	    }
	/* This should neve happen in this instance */
	else if (status < -1)
	    {
	    G_fatal_error("Internal Error... Punting\n"); 
	     }
	(void)ShowBusy(XtDisplay(w), busy);
	SaveCellFile(editor);
	(void)HideBusy(XtDisplay(w), busy);
	}
    else
	{
	GetMouse(editor, &x, &y);
	MovePopup(dialogShell, x, y);
	XtPopup(dialogShell, XtGrabNone);
	}
    }

void DoneProc(w, client, call)
    Widget w;
    XtPointer client;
    XtPointer call;
    {
    char *file;
    char *cellPath;
    char **cellList;
    int status = 0;
    int foo;

    XtPopdown(dialogShell);
    XFlush(XtDisplay(w));

    file = G_store(XawDialogGetValueString(dialogBox));
    status = SetSaveFile(editor, file);
    if (status == 0)
	{
	G_warning("You must first load a file");
	return;
	}
    else if (status < -1)
	{
	/* Message box
	 * Illeagal Filename.
	 * Dismiss, repostDB
	 */
	 return;
	 }
    (void)ShowBusy(XtDisplay(w), busy);
    SaveCellFile(editor);
    /* update the load menu */
    cellPath = (char *)G_store(GetCellPath(editor));
    if (cellPath == (char *)NULL)
	{
	G_fatal_error(stderr, "unable to load cell file directory.\n");
	}
    cellList = (char **)MakeFileList(cellPath, &foo);
    NewList(cellList);
    (void)HideBusy(XtDisplay(w), busy);
    }

static void CancelProc(w, client, call)
    Widget w;
    XtPointer client;
    XtPointer call;
    {
    XtPopdown(dialogShell);
    }
