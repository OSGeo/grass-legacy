
/* THIS FILE WAS CREATED AUTOMATICALLY - DO NOT EDIT IT */

static char *_Xg_printcomand = "cat %s | /bin/lp";

#include <Interact.h>
#include <Pixel.h>
#include <Xm/XmP.h>
#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/Command.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/MwmUtil.h>
#include <Xm/PanedW.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/SelectioB.h>
#include <Xm/SeparatoG.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <varargs.h>

#ifndef mips
#include <unistd.h>
#endif
extern int errno;
#include <sys/stat.h>
#include <stdio.h>

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
    InteractorCallbackStruct *xgb =
        (InteractorCallbackStruct *)cad;
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
     Atom protocol;
     Arg al[10];
     int ac = 0;
     XmString xms;

     xms = XmStringCreateSimple("Enter file name:");
     XtSetArg(al[ac], XmNpromptLabelString, xms); ac++;
     dialog = XgCreateInteractorPromptDialog((Widget)cld,
	 "XGRASS Editor Save As", al, ac);

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

    XtUnmanageChild(XgInteractorGetChild(dialog,XmINTERACT_HELP_BUTTON));
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
    XmString xms = XmStringCreateSimple("XGRASS Editor Load Dialog");;

    fsb = XmCreateFileSelectionDialog(w, "load_dialog", NULL, 0);
    XtVaSetValues(fsb, XmNdialogTitle, xms, XmNautoUnmanage, True, NULL);
    XtAddCallback(fsb, XmNokCallback, __XgLoadFile, cld);
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
    XmString xms = XmStringCreateSimple("XGRASS Editor Insert Dialog");;

    fsb = XmCreateFileSelectionDialog(w, "insert_dialog", NULL, 0);
    XtVaSetValues(fsb, XmNdialogTitle, xms, XmNautoUnmanage, True, NULL);
    XtAddCallback(fsb, XmNokCallback, __XgInsertFile, cld);
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
    InteractorCallbackStruct *xgb =
	(InteractorCallbackStruct *)cad;
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
    Arg al[10];
    int ac = 0;
    char *command;
    XmString xms;
    XmString xms2;

    xms = XmStringCreateLtoR("Enter print command\n('%s' will get replaced with a temporary file name):",XmSTRING_DEFAULT_CHARSET);
    XtSetArg(al[ac], XmNpromptLabelString, xms); ac++;

    command = (char *)getenv("XGRASSPRINTCOM");
    if ( command != NULL ) {
	xms2 = XmStringCreateSimple(command);
	XtSetArg(al[ac], XmNtextString, xms2); ac++;
    } else {
	xms2 = XmStringCreateSimple(_Xg_printcomand);
	XtSetArg(al[ac], XmNtextString, xms2); ac++;
    }
    dialog = XgCreateInteractorPromptDialog((Widget)cld,
        "XGRASS Editor Save As", al, ac);
    XmStringFree(xms);
    if ( command != NULL )
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
printf("Help\n");
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
    Arg al[5];
    int ac = 0;
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

    main_window = XmCreateMainWindow(w,"main_window",NULL,0);

    if ( command != NULL ) {
        com_window = XmCreateLabelGadget(main_window, command, NULL, 0);
        XtManageChild(com_window);
    }
    menu_bar = XmCreateMenuBar(main_window,"menu_bar",NULL,0);
    XtManageChild(menu_bar);

    /* create the scrolled text window & insert the text */

    frame = XmCreateFrame(main_window, "frame", NULL, 0);
    XtManageChild(frame);

    *textw = XmCreateScrolledText(frame,"editor_text", NULL, 0);
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

    XmTextInsert(*textw, XmTextGetLastPosition(*textw), string);

    XtManageChild(*textw);

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

        xmstring = XmStringCreateSimple("Save File...");
        save_file = XtVaCreateManagedWidget("save_file",
            xmCascadeButtonWidgetClass, file_menu,
            XmNlabelString, xmstring,
            XmNmnemonic, 'F',
            XmNaccelerator, "<Key>F",
            NULL);
        XmStringFree(xmstring);

        XtAddCallback(save_file, XmNactivateCallback,
            _XgEditorSaveAs, (XtPointer)*textw);

        xmstring = XmStringCreateSimple("Load File...");
        load_file = XtVaCreateManagedWidget("load_file",
            xmCascadeButtonWidgetClass, file_menu,
            XmNlabelString, xmstring,
            XmNmnemonic, 'L',
            XmNaccelerator, "<Key>L",
            NULL);
        XmStringFree(xmstring);

        XtAddCallback(load_file, XmNactivateCallback,
            _XgEditorLoad, (XtPointer)*textw);

        xmstring = XmStringCreateSimple("Insert From File...");
        insert_file = XtVaCreateManagedWidget("insert_from_file",
            xmCascadeButtonWidgetClass, file_menu,
            XmNlabelString, xmstring,
            XmNmnemonic, 'I',
            XmNaccelerator, "<Key>I",
            NULL);
        XmStringFree(xmstring);

        XtAddCallback(insert_file, XmNactivateCallback,
            _XgEditorInsert, (XtPointer)*textw);

        XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, file_menu, NULL);

        xmstring = XmStringCreateSimple("Print...");
        print = XtVaCreateManagedWidget("print",
            xmCascadeButtonWidgetClass, file_menu,
            XmNlabelString, xmstring,
            XmNmnemonic, 'A',
            XmNaccelerator, "<Key>A",
            NULL);
        XmStringFree(xmstring);

        XtAddCallback(print, XmNactivateCallback,
            _XgEditorPrint, (XtPointer)*textw);
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
            _XgEditorCancel, (XtPointer)w);

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

        xmstring = XmStringCreateSimple("Cut Selection");
        cut = XtVaCreateManagedWidget("cut",
            xmCascadeButtonWidgetClass, edit_menu,
            XmNlabelString, xmstring,
            XmNmnemonic, 'C',
            XmNaccelerator, "<Key>C",
            NULL);
        XmStringFree(xmstring);

        XtAddCallback(cut, XmNactivateCallback,
            _XgEditorCut, (XtPointer)*textw);

        xmstring = XmStringCreateSimple("Copy Selection");
        copy = XtVaCreateManagedWidget("copy",
            xmCascadeButtonWidgetClass, edit_menu,
            XmNlabelString, xmstring,
            XmNmnemonic, 'o',
            XmNaccelerator, "<Key>o",
            NULL);
        XmStringFree(xmstring);

        XtAddCallback(copy, XmNactivateCallback,
            _XgEditorCopy, (XtPointer)*textw);

        xmstring = XmStringCreateSimple("Paste Selection");
        paste = XtVaCreateManagedWidget("paste",
            xmCascadeButtonWidgetClass, edit_menu,
            XmNlabelString, xmstring,
            XmNmnemonic, 'P',
            XmNaccelerator, "<Key>P",
            NULL);
        XmStringFree(xmstring);

        XtAddCallback(paste, XmNactivateCallback,
            _XgEditorPaste, (XtPointer)*textw);

        XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, edit_menu, NULL);

        xmstring = XmStringCreateSimple("Clear Selection");
        clear = XtVaCreateManagedWidget("clear",
            xmCascadeButtonWidgetClass, edit_menu,
            XmNlabelString, xmstring,
            XmNmnemonic, 'l',
            XmNaccelerator, "<Key>l",
            NULL);
        XmStringFree(xmstring);

        XtAddCallback(clear, XmNactivateCallback,
            _XgEditorClear, (XtPointer)*textw);

        xmstring = XmStringCreateSimple("Help");
        help_menu_button = XtVaCreateManagedWidget("help",
            xmCascadeButtonWidgetClass, menu_bar,
            XmNlabelString, xmstring,
            XmNmnemonic, 'H',
            XmNaccelerator, "Meta<Key>H",
            NULL);
        XmStringFree(xmstring);

        XtAddCallback(help_menu_button, XmNactivateCallback,
            _XgEditorHelp, (XtPointer)main_window);

    XmMainWindowSetAreas(main_window, menu_bar, com_window, NULL, NULL, frame);
    XtManageChild(main_window);

    XtManageChild(w);

    return w;
}
