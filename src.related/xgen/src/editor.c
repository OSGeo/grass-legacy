
static char *_Xg_printcommand = "lpr %s";


#include "xgen.h"

static void
#ifdef _NO_PROTO
XgWMClientMessage(w, cld, msg)
Widget w;
XtPointer cld;
XClientMessageEvent *msg;
#else
XgWMClientMessage(w, cld, msg)
Widget w;
XtPointer cld;
XClientMessageEvent *msg;
#endif
{
    Widget shell = (Widget)cld;
    char  *str;
    int   message = msg->data.l[0];
    Atom  WM_DELETE_WINDOW;

    if (msg->type != ClientMessage)
        return;

    WM_DELETE_WINDOW = XmInternAtom(msg->display, "WM_DELETE_WINDOW", False);

    if (message == WM_DELETE_WINDOW) {
	XtPopdown(shell);
    }
}

/* 
 * Save functionality 
 */

#ifdef _NO_PROTO
__XgSaveToFile(text, file)
Widget text;
char *file;
#else
__XgSaveToFile( Widget text, char *file)
#endif
{
    FILE *fp, *fopen();
    char *string;
    int length = 0;
    char errorbuf[1024];
    extern char *sys_errlist[];

    string = XmTextGetString(text);
    length = XmTextGetLastPosition(text);
    if ( string == NULL || *string == NULL ) {
	return;
    }

    fp = fopen(file, "w");
    if ( fp == NULL ) {
	sprintf(errorbuf,"\"%s\": %s", file, sys_errlist[errno]);
	XgWarningDialog(text,errorbuf);
	return;
    }
    fwrite(string, sizeof(char), length, fp);
    fclose(fp);
}

static void
#ifdef _NO_PROTO
__XgSaveAsFile(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
__XgSaveAsFile(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    XmSelectionBoxCallbackStruct *xgb =
        (XmSelectionBoxCallbackStruct *)cad;
    char *ptr, *file, *directory, *path;
    char errorbuf[1024];
    extern char *sys_errlist[];
    struct stat sbuf;

    XmStringGetLtoR(xgb->value,XmSTRING_DEFAULT_CHARSET,&path);
    /* make directory point to the directory part */
    file = XtNewString(path);
    ptr = (char *)strrchr(path,'/');
    if ( ptr == NULL ) {
	directory = "./";
    } else {
	*ptr = '\0';
	directory = path;
    }

    /* is the directory path writeable? */
    if ( access(directory, W_OK) == -1 ) {
	sprintf(errorbuf,"\"%s\": %s", directory, sys_errlist[errno]);
	XgWarningDialog(w,errorbuf);
	return;
    }
    /* does the file already exist? */
    if ( access(file, F_OK) != -1 ) {
	/* a legitimate, but already existing file... */
	if ( access(file, W_OK) != -1 ) {
	    sprintf(errorbuf,"File \"%s\" exists, overwrite?",file);
	    if ( XgYesNo(w, errorbuf) ) {
		__XgSaveToFile((Widget) cld, file);
	    }
	} else {
	    sprintf(errorbuf,"\"%s\": %s",file, sys_errlist[errno]);
	    XgWarningDialog(w,errorbuf);
	}
    } else {
	/* everything is OK, write the file */
	__XgSaveToFile((Widget) cld, file);
    }

}

static void
#ifdef _NO_PROTO
_XgEditorSaveAs(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XgEditorSaveAs(Widget w, XtPointer cld, XtPointer cad)
#endif
{
     Widget dialog;
     Widget shell;
     Widget text;
     Atom protocol;
     int n = 0;
     XmString xms;

     xms = XmStringCreateSimple("Enter file name:");
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
     XtSetArg(args[n], XmNselectionLabelString, xms); n++;
     dialog = XmCreatePromptDialog((Widget)cld,
         "Save As", args, n);

    shell = XtParent(dialog);
    if ( XmIsMotifWMRunning(shell) ) {
        unsigned int decor_flags;

        decor_flags = MWM_DECOR_BORDER;

        XtVaSetValues(shell,
            XmNmwmDecorations, decor_flags,
            NULL);
    }

    XtAddCallback(dialog, XmNokCallback, __XgSaveAsFile, cld);
    XtAddCallback(dialog, XmNcancelCallback, XtUnmanageChild, dialog);

    XtSetValues(XmSelectionBoxGetChild(dialog,XmDIALOG_TEXT), args, n);
    XtUnmanageChild(XmSelectionBoxGetChild(dialog,XmDIALOG_HELP_BUTTON));
    XtManageChild(dialog);
}

/* 
 * Load functionality 
 */

static void
#ifdef _NO_PROTO
__XgLoadFile(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
__XgLoadFile(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget text = (Widget)cld;
    XmFileSelectionBoxCallbackStruct *fsb =
	(XmFileSelectionBoxCallbackStruct *)cad;
    char *file;
    int acstat;
    FILE *fp, *fopen();
    struct stat sbuf;
    char errorbuf[1024];
    char *buf;
    int buflen;
    int bytes;
    Boolean first = True;

    XmStringGetLtoR(fsb->value,XmSTRING_DEFAULT_CHARSET,&file);

    if ( stat(file, &sbuf) == -1 || (acstat = access(file, R_OK)) == -1 ) {
        if ( acstat == -1 ) {
            sprintf(errorbuf,"Can't load from \"%s\", not readable.", file);
            XgWarningDialog(text,errorbuf);
        } else {
            sprintf(errorbuf,"Can't load from \"%s\", try again.",
                file);
            XgWarningDialog(text,errorbuf);
        }
        return;
    }
    if ((sbuf.st_mode & S_IFMT) == S_IFDIR) {
        sprintf(errorbuf,"Cannot load from \"%s\", it is a directory!",file);
        XgWarningDialog(text,errorbuf);
        return;
    }
    buflen = sbuf.st_size;

    if ( buflen > 0 ) {
	fp = fopen(file, "r");
        buf = XtMalloc(buflen);
	bytes = fread(buf,sizeof(char),buflen,fp);
	buf[bytes] = '\0';
	fclose(fp);
        if ( bytes ) {
            XmTextReplace(text,0,XmTextGetLastPosition(text),buf);
        }
    }
}

static void
#ifdef _NO_PROTO
_XgEditorLoad(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XgEditorLoad(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget fsb;
    XmString xms = XmStringCreateSimple("XGRASS Editor Load Dialog");
    int n = 0;

    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    fsb = XmCreateFileSelectionDialog(w, "load_dialog", args, n);
    XtVaSetValues(fsb, XmNdialogTitle, xms, XmNautoUnmanage, True, NULL);
    XtAddCallback(fsb, XmNokCallback, __XgLoadFile, cld);
    XtSetValues(XmFileSelectionBoxGetChild(fsb,XmDIALOG_TEXT), args, n);
    XtSetValues(XmFileSelectionBoxGetChild(fsb,XmDIALOG_FILTER_TEXT), args, n);
    XtSetValues(XmFileSelectionBoxGetChild(fsb,XmDIALOG_LIST), args, n);
    XtSetValues(XmFileSelectionBoxGetChild(fsb,XmDIALOG_DIR_LIST), args, n);
    XtManageChild(fsb);
}

/* 
 * Insert functionality 
 */


static void
#ifdef _NO_PROTO
__XgInsertFile(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
__XgInsertFile(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget text = (Widget)cld;
    XmFileSelectionBoxCallbackStruct *fsb =
	(XmFileSelectionBoxCallbackStruct *)cad;
    char *file;
    int acstat;
    FILE *fp, *fopen();
    struct stat sbuf;
    char errorbuf[1024];
    char *buf;
    int buflen;
    int bytes;
    Boolean first = True;

    XmStringGetLtoR(fsb->value,XmSTRING_DEFAULT_CHARSET,&file);

    if ( stat(file, &sbuf) == -1 || (acstat = access(file, R_OK)) == -1 ) {
        if ( acstat == -1 ) {
            sprintf(errorbuf,"Can't insert from \"%s\", not readable.", file);
            XgWarningDialog(text,errorbuf);
        } else {
            sprintf(errorbuf,"Can't insert from \"%s\", try again.",
                file);
            XgWarningDialog(text,errorbuf);
        }
        return;
    }
    if ((sbuf.st_mode & S_IFMT) == S_IFDIR) {
        sprintf(errorbuf,"Cannot insert from \"%s\", it is a directory!",file);
        XgWarningDialog(text,errorbuf);
        return;
    }
    buflen = sbuf.st_size;

    if ( buflen > 0 ) {
	fp = fopen(file, "r");
        buf = XtMalloc(buflen);
	bytes = fread(buf,sizeof(char),buflen,fp);
	buf[bytes] = '\0';
	fclose(fp);
        if ( bytes ) {
            XmTextInsert(text,XmTextGetInsertionPosition(text),buf);
        }
    }
}

static void
#ifdef _NO_PROTO
_XgEditorInsert(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XgEditorInsert(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget fsb;
    XmString xms = XmStringCreateSimple("Insert Dialog");
    int n = 0;
 
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    fsb = XmCreateFileSelectionDialog(w, "insert_dialog", args, n);
    XtVaSetValues(fsb, XmNdialogTitle, xms, XmNautoUnmanage, True, NULL);
    XtAddCallback(fsb, XmNokCallback, __XgInsertFile, cld);
    XtSetValues(XmFileSelectionBoxGetChild(fsb,XmDIALOG_TEXT), args, n);
    XtSetValues(XmFileSelectionBoxGetChild(fsb,XmDIALOG_FILTER_TEXT), args, n);
    XtSetValues(XmFileSelectionBoxGetChild(fsb,XmDIALOG_LIST), args, n);
    XtSetValues(XtParent(XmFileSelectionBoxGetChild(fsb,XmDIALOG_LIST)), args, n);
    XtSetValues(XmFileSelectionBoxGetChild(fsb,XmDIALOG_DIR_LIST), args, n);
    XtSetValues(XtParent(XmFileSelectionBoxGetChild(fsb,XmDIALOG_DIR_LIST)), args, n);
    XtManageChild(fsb);
}

/* 
 * Print functionality 
 */

/*
 * This should be replaced with strstr when all machines have it.
 */
Boolean
#ifdef _NO_PROTO
ContainsString(buffer, pattern)
char *buffer, *pattern;
#else
ContainsString( char *buffer, char *pattern)
#endif
{
    register int i = 0, j = 0;

    while (buffer[i] != '\0') {
        while (buffer[i++] == pattern[j++])
            if (pattern[j] == '\0')
                return(True);
        i = i - j + 1;
        j = 0;
    }
    return(False);
}

static void
#ifdef _NO_PROTO
__XgPrintFile(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
__XgPrintFile(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    XmSelectionBoxCallbackStruct *xgb =
        (XmSelectionBoxCallbackStruct *)cad;
    char *command;
    char *templ = "/tmp/xgprintXXXXXX";
    char *tempfile;
    char pbuf[1024], buf[1024];

    XmStringGetLtoR(xgb->value,XmSTRING_DEFAULT_CHARSET,&command);

    if ( command == NULL ) 
        return;

    tempfile = (char *)mktemp(templ);

    /* if the command contains %s then sprint tempfile name into the command */
    if ( ContainsString(command,"%s") ) {
        sprintf(pbuf,command,tempfile);
    } else { /* if not, assume "command tempfile" will work... */
        sprintf(pbuf,"%s %s",command,tempfile);
    }

    sprintf(buf,"%s 2>&1", pbuf);

    __XgSaveToFile((Widget)cld, tempfile);

    system(buf);

}

static void
#ifdef _NO_PROTO
_XgEditorPrint(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XgEditorPrint(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget dialog;
    Widget shell;
    Atom protocol;
    int n = 0;
    XmString xms;
    XmString xms2;
    char *printcom;

    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    xms = XmStringCreateLtoR("Enter print command\n('%s' will get replaced with a temporary file name):", SDC);
    XtSetArg(args[n], XmNselectionLabelString, xms); n++;

    printcom = (char *)getenv("XGEN_PRINT");
    if ( printcom != NULL ) {
	xms2 = XmStringCreateSimple(printcom);
	XtSetArg(args[n], XmNtextString, xms2); n++;
    } else {
	xms2 = XmStringCreateSimple(_Xg_printcommand);
	XtSetArg(args[n], XmNtextString, xms2); n++;
    }
    dialog = XmCreatePromptDialog((Widget)cld,
        "Save As", args, n);
    XmStringFree(xms);
    if ( printcom != NULL )
	XmStringFree(xms2);


   shell = XtParent(dialog);
   if ( XmIsMotifWMRunning(shell) ) {
       unsigned int decor_flags;

       decor_flags = MWM_DECOR_BORDER;

       XtVaSetValues(shell,
           XmNmwmDecorations, decor_flags,
           NULL);
   }

   XtAddCallback(dialog, XmNokCallback, __XgPrintFile, cld);
   XtAddCallback(dialog, XmNcancelCallback, XtUnmanageChild, dialog);
   XtSetValues(XmSelectionBoxGetChild(dialog,XmDIALOG_TEXT), args, n);

   XtManageChild(dialog);

}
/*
 * Quit functionality
 */

static void
#ifdef _NO_PROTO
_XgEditorCancel(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XgEditorCancel(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    XtPopdown((Widget)cld);
}

static void
#ifdef _NO_PROTO
_XgEditorCut(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XgEditorCut(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    if ( XmTextCut((Widget)cld,CurrentTime) == False ) {
        XgWarningDialog((Widget)cld,"Nothing to cut!");
        return;
    }
}

static void
#ifdef _NO_PROTO
_XgEditorCopy(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XgEditorCopy(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    if ( XmTextCopy((Widget)cld,CurrentTime) == False ) {
        XgWarningDialog((Widget)cld,"Nothing to copy!");
        return;
    }
}

static void
#ifdef _NO_PROTO
_XgEditorPaste(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XgEditorPaste(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    if ( XmTextPaste((Widget)cld) == False ) {
        XgWarningDialog((Widget)cld,"Nothing to paste!");
        return;
    }
}

static void
#ifdef _NO_PROTO
_XgEditorClear(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XgEditorClear(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    XmTextClearSelection((Widget)cld,CurrentTime);
}

static void
#ifdef _NO_PROTO
_XgEditorHelp(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XgEditorHelp(Widget w, XtPointer cld, XtPointer cad)
#endif
{
fprintf (stdout,"Help\n");
}


/****************************************************************/
Widget
#ifdef _NO_PROTO
XgEditor(widget, string, title, command, textw)
    Widget                          widget;
    char *string;
    char *title;
    char *command;
    Widget *textw;
#else
XgEditor(Widget widget,
               char *string, char *title, char *command, Widget *textw)
#endif
{
    Widget w;
    Widget main_window;
    Widget com_window;
    Widget menu_bar;
    Widget frame;
    Widget sep;
    Widget file_menu;
    Widget file_menu_button;
    Widget save_file;
    Widget load_file;
    Widget insert_file;
    Widget print;
    Widget quit;
    Widget edit_menu;
    Widget edit_menu_button;
    Widget cut;
    Widget copy;
    Widget paste;
    Widget clear;
    Widget help_menu;
    Widget help_menu_button;
    XmString xmstring;
    int n = 0;
    Atom protocol;

    w =  XtVaCreatePopupShell(title, vendorShellWidgetClass, widget,
	XmNinput,True, 
	XmNwindowGroup, XtWindow(widget), 
	XmNtransient, True, 
	XmNmappedWhenManaged,False,
	XmNallowShellResize,True, NULL);

    protocol = XmInternAtom(XtDisplay(w), "WM_DELETE_WINDOW", False);
    XmAddWMProtocols(w, &protocol, 1);
    XtAddEventHandler(w, NoEventMask, True, XgWMClientMessage, w);

    if ( XmIsMotifWMRunning(w) ) {
	unsigned int decor_flags, func_flags;

	decor_flags = MWM_DECOR_BORDER | MWM_DECOR_RESIZEH;
	decor_flags |= MWM_DECOR_TITLE | MWM_DECOR_MENU;

	func_flags = MWM_FUNC_CLOSE | MWM_FUNC_RESIZE;
	func_flags |= MWM_FUNC_MOVE;

	XtVaSetValues(w,
	    XmNmwmDecorations, decor_flags,
	    XmNmwmFunctions, func_flags,
	    NULL);
    }
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);

    main_window = XmCreateMainWindow(w,"main_window",args, n);

    if ( command != NULL ) {
        com_window = XmCreateLabelGadget(main_window, command, args, n);
        XtManageChild(com_window);
    }
    menu_bar = XmCreateMenuBar(main_window,"menu_bar",args, n);
    XtManageChild(menu_bar);

    /* create the scrolled text window & insert the text */

    frame = XmCreateFrame(main_window, "frame", args, n);
    XtManageChild(frame);

    *textw = XmCreateScrolledText(frame,"editor_text", args, n);
    XtVaSetValues(*textw,
        XmNrows, 10,
        XmNcolumns, 24,
        /*
        XmNresizeWidth, False,
        XmNresizeHeight, False,
        */
        XmNscrollVertical, True,
        XmNscrollHorizontal, True,
        XmNeditMode, XmMULTI_LINE_EDIT, NULL);
 
    if ( xgenGD.g_edfs ) {
        XmFontList fl = XmFontListCreate(xgenGD.g_edfs, SDC);

        XtVaSetValues(*textw, XmNfontList, fl, NULL);
    }

    XmTextInsert(*textw, XmTextGetLastPosition(*textw), string);

    XtManageChild(*textw);

        /* create pulldown menu system */

        file_menu = XmCreatePulldownMenu(menu_bar, "File", args, n);

        xmstring = XmStringCreateSimple("File");
        file_menu_button = XmCreateCascadeButton(menu_bar, "file", args, n);
        XtVaSetValues(file_menu_button,
            XmNlabelString, xmstring,
            XmNmnemonic, 'F',
            XmNaccelerator, "Meta<Key>F",
            XmNsubMenuId, file_menu,
            NULL);
        XtManageChild(file_menu_button);
        XmStringFree(xmstring);

        xmstring = XmStringCreateSimple("Save File...");
        save_file = XmCreateCascadeButton(file_menu, "save_file", args, n);
        XtVaSetValues(save_file,
            XmNlabelString, xmstring,
            XmNmnemonic, 'F',
            XmNaccelerator, "<Key>F",
            NULL);
	XtManageChild(save_file);
        XmStringFree(xmstring);

        XtAddCallback(save_file, XmNactivateCallback,
            _XgEditorSaveAs, (XtPointer)*textw);

        xmstring = XmStringCreateSimple("Load File...");
        load_file = XmCreateCascadeButton(file_menu, "load_file", args, n);
        XtVaSetValues(load_file,
            XmNlabelString, xmstring,
            XmNmnemonic, 'L',
            XmNaccelerator, "<Key>L",
            NULL);
	XtManageChild(load_file);
        XmStringFree(xmstring);

        XtAddCallback(load_file, XmNactivateCallback,
            _XgEditorLoad, (XtPointer)*textw);

        xmstring = XmStringCreateSimple("Insert From File...");
        insert_file = XmCreateCascadeButton(file_menu, "insert_file", args, n);
        XtVaSetValues(insert_file,
            XmNlabelString, xmstring,
            XmNmnemonic, 'I',
            XmNaccelerator, "<Key>I",
            NULL);
	XtManageChild(insert_file);
        XmStringFree(xmstring);

        XtAddCallback(insert_file, XmNactivateCallback,
            _XgEditorInsert, (XtPointer)*textw);

        sep = XmCreateSeparator(file_menu,"sep", args, n);
	XtManageChild(sep);

        xmstring = XmStringCreateSimple("Print...");
        print = XmCreateCascadeButton(file_menu, "print", args, n);
        XtVaSetValues(print,
            XmNlabelString, xmstring,
            XmNmnemonic, 'A',
            XmNaccelerator, "<Key>A",
            NULL);
	XtManageChild(print);
        XmStringFree(xmstring);

        XtAddCallback(print, XmNactivateCallback,
            _XgEditorPrint, (XtPointer)*textw);

        sep = XmCreateSeparator(file_menu,"sep", args, n);
	XtManageChild(sep);

        xmstring = XmStringCreateSimple("Quit");
        quit = XmCreateCascadeButton(file_menu, "quit", args, n);
        XtVaSetValues(quit,
            XmNlabelString, xmstring,
            XmNmnemonic, 'Q',
            XmNaccelerator, "Ctrl<Key>c",
            XmNacceleratorText, XmStringCreateSimple("Ctrl-C"),
            NULL);
	XtManageChild(quit);
        XmStringFree(xmstring);

        XtAddCallback(quit, XmNactivateCallback,
            _XgEditorCancel, (XtPointer)w);

        edit_menu = XmCreatePulldownMenu(menu_bar, "Edit", args, n);

        xmstring = XmStringCreateSimple("Edit");
        edit_menu_button = XmCreateCascadeButton(menu_bar, "edit_menu", args, n);
        XtVaSetValues(edit_menu_button,
            XmNlabelString, xmstring,
            XmNmnemonic, 'E',
            XmNaccelerator, "Meta<Key>E",
            XmNsubMenuId, edit_menu,
            NULL);
	XtManageChild(edit_menu_button);
        XmStringFree(xmstring);

        xmstring = XmStringCreateSimple("Cut Selection");
        cut = XmCreateCascadeButton(edit_menu, "File", args, n);
        XtVaSetValues(cut,
            XmNlabelString, xmstring,
            XmNmnemonic, 'C',
            XmNaccelerator, "<Key>C",
            NULL);
	XtManageChild(cut);
        XmStringFree(xmstring);

        XtAddCallback(cut, XmNactivateCallback,
            _XgEditorCut, (XtPointer)*textw);

        xmstring = XmStringCreateSimple("Copy Selection");
        copy = XmCreateCascadeButton(edit_menu, "File", args, n);
        XtVaSetValues(copy,
            XmNlabelString, xmstring,
            XmNmnemonic, 'o',
            XmNaccelerator, "<Key>o",
            NULL);
	XtManageChild(copy);
        XmStringFree(xmstring);

        XtAddCallback(copy, XmNactivateCallback,
            _XgEditorCopy, (XtPointer)*textw);

        xmstring = XmStringCreateSimple("Paste Selection");
        paste = XmCreateCascadeButton(edit_menu, "File", args, n);
        XtVaSetValues(paste,
            XmNlabelString, xmstring,
            XmNmnemonic, 'P',
            XmNaccelerator, "<Key>P",
            NULL);
	XtManageChild(paste);
        XmStringFree(xmstring);

        XtAddCallback(paste, XmNactivateCallback,
            _XgEditorPaste, (XtPointer)*textw);

        sep = XmCreateSeparator(edit_menu,"sep", args, n);
	XtManageChild(sep);

        xmstring = XmStringCreateSimple("Clear Selection");
        clear = XmCreateCascadeButton(edit_menu, "File", args, n);
        XtVaSetValues(clear,
            XmNlabelString, xmstring,
            XmNmnemonic, 'l',
            XmNaccelerator, "<Key>l",
            NULL);
	XtManageChild(clear);
        XmStringFree(xmstring);

        XtAddCallback(clear, XmNactivateCallback,
            _XgEditorClear, (XtPointer)*textw);

        xmstring = XmStringCreateSimple("Help");
        help_menu_button = XmCreateCascadeButton(menu_bar, "File", args, n);
        XtVaSetValues(help_menu_button,
            XmNlabelString, xmstring,
            XmNmnemonic, 'H',
            XmNaccelerator, "Meta<Key>H",
            NULL);
	XtManageChild(help_menu_button);
        XmStringFree(xmstring);

        XtAddCallback(help_menu_button, XmNactivateCallback,
            _XgEditorHelp, (XtPointer)main_window);

    XmMainWindowSetAreas(main_window, menu_bar, com_window, NULL, NULL, frame);
    XtManageChild(main_window);

    XtManageChild(w);

    return w;
}
