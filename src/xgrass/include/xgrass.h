/**********************************************************************
   xgrass.h       - main header file
 *********************************************************************/
#ifndef _XGRASS_H
#define _XGRASS_H

#include "std_incs.h"

#include "Interact.h"
#include "Region.h"

#include "hourglass.h"
#include "patchlevel.h"
#include "menu_item.h"
#include "xg_history.h"
#include "xgrass_lib.h"
#include "gis.h"

typedef struct _XgrassGlobalData {
    char *progName;         /* program name */
    Widget applShell;       /* application shell widget (toplevel widget) */
    Widget help;            /* help shell widget */
    Widget error;           /* error shell widget */
    Display *display;       /* the display */
    char *session;	    /* name of session file */
    char *database;
    char *location;
    char *mapset;
    int screen;             /* screen info: for X calls */
    Screen *scrptr;         /* for Xt and Xm calls, sigh */
    Colormap cmap;          /* default colormap */    
    XtAppContext appContext;/* the application context */
    XgMenuData menuData;    /* the menu system data (loaded from grass-menu) */
    int history_enabled;    /* is the history mechanism enabled ? */
    Widget historyWidget;   /* the history editor popup */
    Widget historyText;     /* the history editor */
    XgHistoryItemRec *history; /* the actual history... */
    Boolean menuRunning;    /* is the menu system up (also means DML set) */
} XgrassGlobalData;

#define SDC XmSTRING_DEFAULT_CHARSET

#ifdef MAIN
    XgrassGlobalData _XG_Global;
    char errorbuf[1024];
    Boolean verbose;
#else  /* !MAIN */
    extern XgrassGlobalData _XG_Global;
    extern char errorbuf[1024];
    extern Boolean verbose;
#endif /* MAIN */

extern int errno;

void Interrupt();
char *ParseCommand();
char *StrDup();
Widget BuildItem();
char **_XgTokenize();

void _XgrassExit();
void _XgExec();
void _XgExecCapture();
void _XgExecHist();
void _XgExecCaptureHist();
void _XgDbSet();
void _XgMBExec();
void _XgXclip();
void _XgMBXclip();
void _XgHistoryToggle();
void _XgHistoryClear();
void _XgHistoryEdit();
void _XgHistoryReplay();

void _XgHistoryEditorCut();
void _XgHistoryEditorCopy();
void _XgHistoryEditorPaste();
void _XgHistoryEditorClear();
void _XgHistoryEditorCancel();
void _XgHistoryEditorSaveAs();
void _XgHistoryEditorLoad();
void _XgHistoryEditorSaveEdits();
void _XgHistoryEditorPopup();
void __XgFindFilesWithHistoryExtension();
void __XgHistorySaveAsFile();
void __XgHistorySaveAsCancel();
void _XgOverwriteOK();

void _XgWMClientMessage();

void _XgExitOK();

#endif /* _XGRASS_H */
