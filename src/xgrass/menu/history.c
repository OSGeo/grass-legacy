static char rcsid[] = "@(#)XGRASS $Id: history.c,v 0.0.0.1 1992/05/05 14:58:33 kurt Exp kurt $";
/*
 * File: history.c
 *
 * Desc: history mechanism routines
 *
 * Auth: Kurt Buehler
 *
 * Date: Mon Oct 21 13:20:02 CDT 1991
 *
 * Modification History:
 *
 *
 */
#include <string.h>
#include <errno.h>
#include "xgrass.h"

extern char *version;

_XgAddHistoryCommand(w, client_data, event, continue_to_dispatch)
Widget w;
XtPointer client_data;
XEvent *event;
Boolean *continue_to_dispatch;
{
  XgMenuItemRec *mi;

  char *s;
  s = (char *) XgGetCommandString(XtDisplay(w),XtWindow(_XG_Global.applShell));
  if (s) {
	if (*s) {
	    mi = (XgMenuItemRec *)XtMalloc(sizeof(XgMenuItemRec));
	    bzero((char *)mi, sizeof(XgMenuItemRec));
	    mi->arglist = s;
	    __XgHistoryAddItem(mi,XG_HISTORY_EXEC);
	}
  }
}

__XgHistoryAddItem(ptr,type)
XgMenuItemRec *ptr;
int type;
{
    XgHistoryItemRec *item;
    XgHistoryItemRec *hptr;

    if ( _XG_Global.history_enabled == XG_HISTORY_ENABLE ) {
        /* alloc space for the new item */
        item = (XgHistoryItemRec *)XtMalloc(sizeof(XgHistoryItemRec));
        bzero((char *)item,sizeof(XgHistoryItemRec));

        item->type = type;
        item->text = _XgStrDup(ptr->arglist);

#ifdef Undefined
        if ( _XG_Global.historyWidget &&
            XtIsRealized(_XG_Global.historyWidget) ) {
#else
        if ( _XG_Global.historyWidget ) {
#endif
            switch(item->type) {
            case XG_HISTORY_EXEC_CAPTURE:
                sprintf(errorbuf,"xgexec %s\n",item->text);
                break;
            case XG_HISTORY_EXEC:
                sprintf(errorbuf,"%s\n",item->text);
                break;
            case XG_HISTORY_XCLIP:
                sprintf(errorbuf,"xclip %s\n",item->text);
                break;
            }
            XmTextInsert(_XG_Global.historyText,
                XmTextGetLastPosition(_XG_Global.historyText),errorbuf);
        }

        /* make global pointer point to it if it isn't pointing to something */
        if ( _XG_Global.history == NULL ) {
            _XG_Global.history = item;
        } else {
            /* walk to the last item that is defined */
            hptr = _XG_Global.history;
            while ( hptr->next )
                hptr = hptr->next;
            /* link the new element in */
            hptr->next = item;
        }
    }
}

__XgFreeHistoryList()
{
    if ( _XG_Global.history )
        __XgFreeHistItems(_XG_Global.history);
    _XG_Global.history = NULL;
}

__XgFreeHistItems(ptr)
XgHistoryItemRec *ptr;
{
    if ( ptr->next )
        __XgFreeHistItems(ptr->next);
    else
        XtFree(ptr);
}

__XgHistoryReplay()
{
    XgHistoryItemRec *ptr = _XG_Global.history;
    char buf[1024];
    int err = 0;

    if ( ptr && _XG_Global.history_enabled == XG_HISTORY_ENABLE ) {
        while ( ptr ) {
            switch(ptr->type) {
            case XG_HISTORY_EXEC_CAPTURE:
                sprintf(buf,"%s\n",ptr->text);
		XgSystem(_XG_Global.applShell, buf, False, &err, 1);
                break;
            case XG_HISTORY_EXEC:
                sprintf(buf,"%s\n",ptr->text);
		XgSystem(_XG_Global.applShell, buf, True, &err, 1);
                break;
            case XG_HISTORY_XCLIP:
                sprintf(buf,"xclip %s\n",ptr->text);
		XgSystem(_XG_Global.applShell, buf, True, &err, 1);
                break;
            }
            ptr = ptr->next;
        }
    }
}

__XgHistorySetup()
{
    XgHistoryItemRec *ptr;
    Widget shell;
    Widget main_window;
    Widget menu_bar;
    Widget file_menu, file_menu_button;
    Widget quit, save_edits, save_file, load_file;

    Widget edit_menu, edit_menu_button;
    Widget cut, copy, paste, clear;

    Widget frame;
    Widget text;
    XmString xmstring;
    Atom protocol;
    Arg args[15];
    char buf[256];

    if ( _XG_Global.history_enabled == XG_HISTORY_ENABLE ) {

	sprintf(buf, "View/Edit Current History");
	    _XG_Global.historyWidget = 
            XtVaCreatePopupShell(buf,
                vendorShellWidgetClass,
                _XG_Global.applShell,
                XmNinput,True,
		XmNwindowGroup, XtWindow(_XG_Global.applShell),
		XmNtransient, True,
                XmNmappedWhenManaged,False,
                XmNallowShellResize,True,
                NULL);

	shell = _XG_Global.historyWidget;

	protocol = XmInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW", False);
	XmAddWMProtocols(shell, &protocol, 1);
	XtAddEventHandler(shell, NoEventMask, True, _XgWMClientMessage, shell);

        if ( XmIsMotifWMRunning(shell) ) {
            unsigned int decor_flags, func_flags;
      
            decor_flags = MWM_DECOR_BORDER | MWM_DECOR_RESIZEH;
            decor_flags |= MWM_DECOR_TITLE | MWM_DECOR_MENU;

            func_flags = MWM_FUNC_RESIZE;
            func_flags |= MWM_FUNC_MOVE;

            XtVaSetValues(shell,
                XmNmwmDecorations, decor_flags,
                XmNmwmFunctions, func_flags,
                NULL);
        }

        main_window = XmCreateMainWindow(shell,"main_window",NULL,0);
        XtManageChild(main_window);

        menu_bar = XmCreateMenuBar(main_window,"menu_bar",NULL,0);
        XtManageChild(menu_bar);

        /* create the scrolled text window & insert the text */

        frame = XmCreateFrame(main_window, "frame", NULL, 0);
        XtManageChild(frame);

        _XG_Global.historyText = text = 
            XmCreateScrolledText(frame,"text",NULL,0);
        XtManageChild(text);
        XtVaSetValues (text, 
	    XmNrows, 10,
            XmNcolumns, 24,
	    /*
            XmNresizeWidth, False,
            XmNresizeHeight, False,
	    */
            XmNscrollVertical, True,
            XmNscrollHorizontal, True,
            XmNeditMode, XmMULTI_LINE_EDIT, NULL);

        __XgInsertHistoryText(text);

	/* create pulldown menu system */

	file_menu = XmCreatePulldownMenu(menu_bar, "File", NULL, 0);

	xmstring = XmStringCreateSimple("File");
        file_menu_button = XtVaCreateManagedWidget("file",
	    xmCascadeButtonWidgetClass, menu_bar,
	    XmNlabelString, xmstring,
	    XmNmnemonic, 'F',
	    XmNaccelerator, "Meta<Key>F",
	    XmNsubMenuId, file_menu,
	    NULL);
	XmStringFree(xmstring);

	xmstring = XmStringCreateSimple("Save Edits");
	save_edits = XtVaCreateManagedWidget("save_edits",
	    xmCascadeButtonWidgetClass, file_menu,
	    XmNlabelString, xmstring,
	    XmNmnemonic, 'S',
	    XmNaccelerator, "<Key>S",
	    NULL);
	XmStringFree(xmstring);

        XtAddCallback(save_edits, XmNactivateCallback,
            _XgHistoryEditorSaveEdits, NULL);

	xmstring = XmStringCreateSimple("Save File...");
	save_file = XtVaCreateManagedWidget("save_file",
	    xmCascadeButtonWidgetClass, file_menu,
	    XmNlabelString, xmstring,
	    XmNmnemonic, 'F',
	    XmNaccelerator, "<Key>F",
	    NULL);
	XmStringFree(xmstring);

        XtAddCallback(save_file, XmNactivateCallback,
            _XgHistoryEditorSaveAs, NULL);

	xmstring = XmStringCreateSimple("Load File...");
	load_file = XtVaCreateManagedWidget("load_file",
	    xmCascadeButtonWidgetClass, file_menu,
	    XmNlabelString, xmstring,
	    XmNmnemonic, 'L',
	    XmNaccelerator, "<Key>L",
	    NULL);
	XmStringFree(xmstring);

        XtAddCallback(load_file, XmNactivateCallback,
            _XgHistoryEditorLoad, NULL);

	XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, file_menu, NULL);

	xmstring = XmStringCreateSimple("Quit");
	quit = XtVaCreateManagedWidget("quit",
	    xmCascadeButtonWidgetClass, file_menu,
	    XmNlabelString, xmstring,
	    XmNmnemonic, 'Q',
	    XmNaccelerator, "Ctrl<Key>c",
	    XmNacceleratorText, XmStringCreateSimple("Ctrl-C"),
	    NULL);
	XmStringFree(xmstring);

        XtAddCallback(quit, XmNactivateCallback,
            _XgHistoryEditorCancel, NULL);

	edit_menu = XmCreatePulldownMenu(menu_bar, "Edit", NULL, 0);

	xmstring = XmStringCreateSimple("Edit");
        edit_menu_button = XtVaCreateManagedWidget("edit",
	    xmCascadeButtonWidgetClass, menu_bar,
	    XmNlabelString, xmstring,
	    XmNmnemonic, 'E',
	    XmNaccelerator, "Meta<Key>E",
	    XmNsubMenuId, edit_menu,
	    NULL);
	XmStringFree(xmstring);

	xmstring = XmStringCreateSimple("Cut");
	cut = XtVaCreateManagedWidget("cut",
	    xmCascadeButtonWidgetClass, edit_menu,
	    XmNlabelString, xmstring,
	    XmNmnemonic, 'C',
	    XmNaccelerator, "<Key>C",
	    NULL);
	XmStringFree(xmstring);

        XtAddCallback(cut, XmNactivateCallback,
            _XgHistoryEditorCut, NULL);

	xmstring = XmStringCreateSimple("Copy");
	copy = XtVaCreateManagedWidget("copy",
	    xmCascadeButtonWidgetClass, edit_menu,
	    XmNlabelString, xmstring,
	    XmNmnemonic, 'o',
	    XmNaccelerator, "<Key>o",
	    NULL);
	XmStringFree(xmstring);

        XtAddCallback(copy, XmNactivateCallback,
            _XgHistoryEditorCopy, NULL);

	xmstring = XmStringCreateSimple("Paste");
	paste = XtVaCreateManagedWidget("paste",
	    xmCascadeButtonWidgetClass, edit_menu,
	    XmNlabelString, xmstring,
	    XmNmnemonic, 'P',
	    XmNaccelerator, "<Key>P",
	    NULL);
	XmStringFree(xmstring);

        XtAddCallback(paste, XmNactivateCallback,
            _XgHistoryEditorPaste, NULL);

	XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, edit_menu, NULL);

	xmstring = XmStringCreateSimple("Clear");
	clear = XtVaCreateManagedWidget("clear",
	    xmCascadeButtonWidgetClass, edit_menu,
	    XmNlabelString, xmstring,
	    XmNmnemonic, 'l',
	    XmNaccelerator, "<Key>l",
	    NULL);
	XmStringFree(xmstring);

        XtAddCallback(clear, XmNactivateCallback,
            _XgHistoryEditorClear, NULL);
    }
}

__XgHistoryEdit()
{
    XgHistoryItemRec *ptr;
    Widget shell;
    Widget main_window;
    Widget menu_bar;
    Widget file_menu, file_menu_button;
    Widget quit, save_edits, save_file, load_file;

    Widget edit_menu, edit_menu_button;
    Widget cut, copy, paste, clear;

    Widget frame;
    Widget text;
    XmString xmstring;
    Atom protocol;
    Arg args[15];
    char buf[256];

	{
	    Position x,y;
	    Position newX,newY;
	    Dimension width, height;
	    Dimension dWidth, dHeight;

	    XtVaGetValues(_XG_Global.applShell, XmNx, &x, XmNy, &y,
		XmNwidth, &width, XmNheight, &height, NULL);
	    XtManageChild(_XG_Global.historyWidget);
	    XtVaGetValues(_XG_Global.historyWidget, XmNwidth, &dWidth, 
		XmNheight, &dHeight, NULL);

	    newX = x + width/2 - dWidth/2;
	    newY = y + height/2 - dHeight/2;

	    XtMoveWidget(_XG_Global.historyWidget,newX,newY);
	}

        XtPopup(_XG_Global.historyWidget,XtGrabNone);
}

__XgInsertHistoryText(text)
Widget text;
{
    XgHistoryItemRec *ptr;
    for ( ptr = _XG_Global.history; ptr; ptr = ptr->next) {
        if ( ptr->text ) {
            switch(ptr->type) {
            case XG_HISTORY_EXEC_CAPTURE:
                sprintf(errorbuf,"xgexec %s\n",ptr->text);
                break;
            case XG_HISTORY_EXEC:
                sprintf(errorbuf,"%s\n",ptr->text);
                break;
            case XG_HISTORY_XCLIP:
                sprintf(errorbuf,"xclip %s\n",ptr->text);
                break;
            }
            XmTextInsert(text,XmTextGetLastPosition(text),errorbuf);
        }
    }
}

void
_XgHistoryEditorCancel(w,cld,cad)
Widget w;
XtPointer cld, cad;
{
    XtPopdown(_XG_Global.historyWidget);
}

void
_XgHistoryEditorSaveEdits(w,cld,cad)
Widget w;
XtPointer cld, cad;
{
    char *string;
    char **tokens;
    XgHistoryItemRec *item, *hptr;
    int n;

    __XgFreeHistoryList();
    XtVaGetValues(_XG_Global.historyText,XmNvalue,&string,NULL);
    if ( string == NULL ) return;
    tokens = _XgTokenize(string,"\n");
    for ( n = 0; tokens[n]; n++ ) {
        if ( tokens[n][0] ) {
            item = (XgHistoryItemRec *)XtMalloc(sizeof(XgHistoryItemRec));
            bzero((char *)item,sizeof(XgHistoryItemRec));
   
            if ( n == 0 ) {
                _XG_Global.history = hptr = item;
            } else {
                hptr->next = item;
                hptr = hptr->next;
            }
            if ( !strncmp(tokens[n],"xclip",4) ) {
                item->type = XG_HISTORY_XCLIP;
                item->text = _XgStrDup(tokens[n] + strlen("xclip "));
            } else if ( !strncmp(tokens[n],"xgexec ",7) ) {
                item->type = XG_HISTORY_EXEC_CAPTURE;
                item->text = _XgStrDup(tokens[n] + strlen("xgexec "));
	    } else {
                item->type = XG_HISTORY_EXEC;
                item->text = _XgStrDup(tokens[n]);
	    }
        } 
    }
    _XgFreeTokens(tokens);
    __XgClearHistoryEditor();
    __XgInsertHistoryText(_XG_Global.historyText);
}

__XgClearHistoryEditor()
{
    XmTextReplace(_XG_Global.historyText, 0,
	XmTextGetLastPosition(_XG_Global.historyText),"");
}

void
_XgHistoryEditorSaveAs(w,cld,cad)
Widget w;
XtPointer cld, cad;
{
     Widget dialog;
     Widget shell;
     Atom protocol;
     Arg al[10];
     int ac = 0;
     XmString xms;
     char buf[256];

     sprintf(buf,"%s History Save As", version);

     xms = XmStringCreateSimple("Enter new history file name:");
     XtSetArg(al[ac], XmNpromptLabelString, xms); ac++;
     dialog = XgCreateInteractorPromptDialog(_XG_Global.historyText,
	 buf , al, ac);

    shell = XtParent(dialog);
    if ( XmIsMotifWMRunning(shell) ) {
        unsigned int decor_flags;

        decor_flags = MWM_DECOR_BORDER;

        XtVaSetValues(shell,
            XmNmwmDecorations, decor_flags,
            NULL);
    }

     XtAddCallback(dialog, XmNokCallback, __XgHistorySaveAsFile, NULL);
     XtAddCallback(dialog, XmNcancelCallback, __XgHistorySaveAsCancel, NULL);

     XtUnmanageChild(XgInteractorGetChild(dialog,XmINTERACT_HELP_BUTTON));
     XtManageChild(dialog);
}

void 
__XgHistorySaveAsFile(w, cld, cad) 
Widget w;
XtPointer cld, cad;
{
    InteractorCallbackStruct *xgb = 
        (InteractorCallbackStruct *)cad;
    char *path;
    char *ptr;
    char *initfile, *file, *directory;
    struct stat sbuf;

    if ( _XG_Global.history == NULL ) {
        XgWarningDialog(_XG_Global.historyWidget,
	    "No history elements to save!");
        return;
    }

    XmStringGetLtoR(xgb->value,XmSTRING_DEFAULT_CHARSET,&path);

    /* make directory point to the directory part */
    initfile = XtNewString(path);
    ptr = (char *)strrchr(path,'/');
    if ( ptr == NULL ) {
        directory = "./";
    } else {
        *ptr = '\0';
        directory = path;
    }

    /* make sure we have a ".xgh" extension */
    if ( strspn(initfile,".xgh") == 0 ) {
	file = XtMalloc(strlen(initfile) + 5);
	strcpy(file, initfile);
	strcat(file, ".xgh");
    } else {
	file = initfile;
    }

    /* is the directory path writeable? */
    if ( access(directory, W_OK) == -1 ) {
        sprintf(errorbuf,"\"%s\": %s", directory, strerror(errno));
        XgWarningDialog(_XG_Global.historyWidget,errorbuf);
        return;
    }
    /* does the file already exist? */
    if ( access(file, F_OK) != -1 ) {
        /* a legitimate, but already existing file... */
        if ( access(file, W_OK) != -1 ) {
            sprintf(errorbuf,"File \"%s\" exists, overwrite?",file);
            if ( XgYesNo(_XG_Global.historyWidget, errorbuf) ) {
                __XgHistorySaveToFile(file);
            }
        } else {
            sprintf(errorbuf,"\"%s\": %s",file, strerror(errno));
            XgWarningDialog(_XG_Global.historyWidget,errorbuf);
        }
    } else {
        /* everything is OK, write the file */
        __XgHistorySaveToFile(file);
    }
}

void 
__XgHistorySaveAsCancel(w, cld, cad) 
Widget w;
XtPointer cld, cad;
{
    XtUnmanageChild(w);
}

__XgHistorySaveToFile(file)
char *file;
{
    FILE *fp, *fopen();
    XgHistoryItemRec *ptr = _XG_Global.history;

    fp = fopen(file, "w");
    fprintf(fp,": XGRASS_HISTORY_FILE_COOKIE\n");
    while( ptr ) {
        switch(ptr->type) {
        case XG_HISTORY_EXEC_CAPTURE:
            fprintf(fp,"xgexec %s\n",ptr->text);
            break;
        case XG_HISTORY_EXEC:
            fprintf(fp,"%s\n",ptr->text);
            break;
        case XG_HISTORY_XCLIP:
            fprintf(fp,"xclip %s\n",ptr->text);
            break;
        }

        ptr = ptr->next;
    }
    fclose(fp);
}

LoadHistory(file,warn)
char *file;
int warn;
{
    int acstat;
    FILE *fp, *fopen();
    XgHistoryItemRec *ptr, *head = NULL;
    struct stat sbuf;
    char buf[1024];
    Boolean first = True;


    if ( stat(file, &sbuf) == -1 || (acstat = access(file, R_OK)) == -1 ) {
	if (warn) {
        if ( acstat == -1 ) {
	    sprintf(errorbuf,"Can't load from \"%s\", not readable.", file);
	    XgWarningDialog(_XG_Global.historyWidget,errorbuf);
	} else {
	    sprintf(errorbuf,"Can't load from \"%s\", try again.",
		file);
	    XgWarningDialog(_XG_Global.historyWidget,errorbuf);
	}
	}
        return;
    }
    if ((sbuf.st_mode & S_IFMT) == S_IFDIR) {
	if (warn) {
        sprintf(errorbuf,"Cannot load from \"%s\", it is a directory!",file);
        XgWarningDialog(_XG_Global.historyWidget,errorbuf);
	}
        return;
    }

    fp = fopen(file, "r");
    while ( fgets(buf, 1024, fp) ) {
        char *sptr = buf;

        while( *sptr ) {
            if ( *sptr == '\n' ) *sptr = '\0';
            else sptr++;
        }
        if ( first ) {
            if ( strcmp(buf,": XGRASS_HISTORY_FILE_COOKIE")) {
		if (warn) {
		sprintf(errorbuf,"\"%s\", is not a history file!!",file);
                XgWarningDialog(_XG_Global.historyWidget,errorbuf);
		}
                return;
            }
	    first = False;
        } else {
	    if ( head == NULL ) {
	        head = ptr = 
                    (XgHistoryItemRec *)XtMalloc(sizeof(XgHistoryItemRec));
	    } else {
		ptr->next = 
                    (XgHistoryItemRec *)XtMalloc(sizeof(XgHistoryItemRec));
		ptr = ptr->next;
	    }
	    bzero((char *)ptr, sizeof(XgHistoryItemRec));
	    if ( !strncmp(buf,"xclip",4) ) {
		ptr->type = XG_HISTORY_XCLIP;
		ptr->text = (char *)_XgStrDup(buf + strlen("xclip "));
            } else if ( !strncmp(buf,"xgexec ",7) ) {
                ptr->type = XG_HISTORY_EXEC_CAPTURE;
                ptr->text = _XgStrDup(buf + strlen("xgexec "));
	    } else {
		ptr->type = XG_HISTORY_EXEC;
		ptr->text = (char *)_XgStrDup(buf);
	    }
	}
    }
    fclose(fp);
    __XgFreeHistoryList();
    _XG_Global.history = head;
    __XgClearHistoryEditor();
    __XgInsertHistoryText(_XG_Global.historyText);
}

__XgHistoryLoadFile(w, cld, cad)
Widget w;
XtPointer cld, cad;
{
    XmFileSelectionBoxCallbackStruct *fsb =
        (XmFileSelectionBoxCallbackStruct *)cad;
    char *file;

    XmStringGetLtoR(fsb->value,XmSTRING_DEFAULT_CHARSET,&file);

    LoadHistory(file,1);
}

__XgHistoryLoadCancel(w, cld, cad)
Widget w;
XtPointer cld, cad;
{
    XtUnmanageChild(w);
}


void
_XgHistoryEditorLoad(w,cld,cad)
Widget w;
XtPointer cld, cad;
{
     Widget dialog;
     Widget shell;
     Atom protocol;
     char buf[256];

     sprintf(buf,"%s History Save As", version);

     dialog = XmCreateFileSelectionDialog(_XG_Global.historyWidget,
         buf, NULL, 0);

     XtVaSetValues(dialog,
         XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
         XmNpattern, XmStringCreateSimple("*.xgh"),
         NULL);

    shell = XtParent(dialog);
    protocol = XmInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW", False);
    XmAddWMProtocols(shell, &protocol, 1);
    XtAddEventHandler(shell, NoEventMask, True, _XgWMClientMessage, dialog);

    if ( XmIsMotifWMRunning(shell) ) {
            unsigned int decor_flags, func_flags;

            decor_flags = MWM_DECOR_BORDER | MWM_DECOR_RESIZEH;
            decor_flags |= MWM_DECOR_TITLE | MWM_DECOR_MENU;

            func_flags = MWM_FUNC_CLOSE | MWM_FUNC_RESIZE;
            func_flags |= MWM_FUNC_MOVE;

            XtVaSetValues(shell,
                XmNmwmDecorations, decor_flags,
                XmNmwmFunctions, func_flags,
                NULL);
    }

     XtAddCallback(dialog, XmNokCallback, __XgHistoryLoadFile, NULL);
     XtAddCallback(dialog, XmNcancelCallback, __XgHistoryLoadCancel, NULL);

     XtUnmanageChild(XmFileSelectionBoxGetChild(dialog,XmDIALOG_HELP_BUTTON));

     XtManageChild(dialog);

}

void
_XgHistoryEditorCut(w,cld,cad)
Widget w;
XtPointer cld, cad;
{
    if ( XmTextCut(_XG_Global.historyText,CurrentTime) == False ) {
        XgWarningDialog(_XG_Global.historyWidget,"Nothing to cut!");
        return;
    }
}

void
_XgHistoryEditorCopy(w,cld,cad)
Widget w;
XtPointer cld, cad;
{
    if ( XmTextCopy(_XG_Global.historyText,CurrentTime) == False ) {
        XgWarningDialog(_XG_Global.historyWidget,"Nothing to copy!");
        return;
    }
}

void
_XgHistoryEditorPaste(w,cld,cad)
Widget w;
XtPointer cld, cad;
{
    if ( XmTextPaste(_XG_Global.historyText) == False ) {
        XgWarningDialog(_XG_Global.historyWidget,"Nothing to paste!");
        return;
    }
}

void
_XgHistoryEditorClear(w,cld,cad)
Widget w;
XtPointer cld, cad;
{
    XmTextClearSelection(_XG_Global.historyText,CurrentTime);
}
