/***********************************************************************

File     	:	load_menu.c
Function 	:	(void)BuildLoadMenu(parent)
Args	 	:	    Widget parent; -- parent menu button

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	15 February 1990
Last Revised	:
Abstract 	:	Build the loadmenu.
Returns  	:	None.

***********************************************************************/
#include "cell_editor.h"
#include "CellEdit.h"
#include <X11/Xaw/List.h>

static void LoadProc();

static XtCallbackRec loadCalls[] =
    {
    {LoadProc,  NULL},
    {NULL,      NULL},
    };

static Widget loadShell;
static Widget loadList;

void BuildLoadMenu(parent)
    Widget parent;
    {
    char	**cellList;     /* list of cell files */
    char	*cellPath;
    int 	num = 0;

    /* create the popup shell */
    loadShell = (Widget)CreatePopupShell(parent, "loadShell", 0);

    cellPath = (char *)G_store(GetCellPath(editor));
    if (cellPath == (char *)NULL)
	{
	G_fatal_error(stderr, "unable to load cell file directory.\n");
	}
    cellList = (char **)MakeFileList(cellPath, &num);
    if (!num)
       G_fatal_error("There are no cell files in your directory\n");
    loadList = (Widget)CreateList(loadShell, cellList, loadCalls);
    }

static void LoadProc(w, client, calls)
    Widget w;
    XtPointer client;
    XtPointer calls;
    {
    int x, y;
    XawListReturnStruct *file = (XawListReturnStruct *)calls;
    char mess[80];

    /* popdown menu */
    XtPopdown(XtParent(w));
    (void)ShowBusy(XtDisplay(w), busy);
    /* load up the file, this takes awhile */
    LoadCellFile(editor, file->string);
    /* global flag */
    if (!loaded)
	{
	(void)BuildCellStats(editor, file->string);
	loaded = TRUE;
	}
    else
	(void)UpdateCellStats(file->string, 0, 0);
    (void)HideBusy(XtDisplay(w), busy);
    }
