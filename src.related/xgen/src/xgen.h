/**********************************************************************
   xgen.h       - main header file
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
#include <X11/Xos.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <signal.h>
#include <sys/stat.h>
#ifdef sparc 
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#include <sys/wait.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/CutPaste.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/SelectioB.h>
#include <Xm/Separator.h>
#include "Table.h"
#include <Xm/Text.h>
#include <Xm/ToggleB.h>

#include "hourglass.h"
#include "hourglassmask.h"
#include "patchlevel.h"
#include "types.h"

#define MAXARGS 1024
Arg args[MAXARGS];


typedef struct _XgenGlobalData {
    char *scriptFile;       /* name of the input script file */
    char *progName;         /* program name */
    Command *commandList;   /* list of commands still active */
    ToggleInfo *toggleInfo; /* pointer to info on active toggles */
    ListInfo *listInfo;     /* pointer to info on active lists */
    Environ *toplevelEnv;   /* pointer to the application data */
    Environ *currentEnv;    /* pointer to the currently displayed environment */
    Widget applShell;       /* application shell widget (toplevel widget) */
    Widget CBRowCol;        /* control box row column widget */
    Widget help;            /* help shell widget */
    Widget error;           /* error shell widget */
    Display *display;       /* the display */
    int screen;             /* screen info: for X calls */
    Screen *scrptr;         /* for Xt and Xm calls, sigh */
    Colormap cmap;          /* default colormap */    
#ifndef PRE_MOTIF_1_1
    /* I realize this is X11R4 stuff, but everyone with Motif 1.1
     * has X11R4 too (I hope) 
     */
    XtAppContext appContext;/* the application context */
#endif
    XmFontList g_fixed;     /* the "fixed" font info */
    char *g_font;           /* font */
    XFontStruct *g_fs;
    char *g_ffont;          /* fixed font */
    XFontStruct *g_ffs;
    char *g_bg;             /* background color */
    XColor g_bgs;
    char *g_fg;             /* foreground color */
    XColor g_fgs;
    char *g_bgpix;          /* background pixmap */
    Pixmap g_bgpm;
    char *g_ts;             /* top shadow color */
    XColor g_tss;
    char *g_tspix;          /* top shadow pixmap */
    Pixmap g_tspm;
    char *g_bs;             /* bottom shadow color */
    XColor g_bss;
    char *g_bspix;          /* bottom shadow pixmap */
    Pixmap g_bspm;
} XgenGlobalData;

#define NOFONTS 0
#define FONTS   1

#define SDC XmSTRING_DEFAULT_CHARSET

extern XgenGlobalData xgenGD;
extern char errorbuf[1024];
extern Boolean verbose;
extern Boolean parse_only;
extern Boolean nocpp;
extern Boolean nocontrol;

extern int errno;
void XgenFatalError();
void XgenFatalWarning();
void XgenWarning();

void XgenIntr();
void XgenExit();

void ExpandObjectValues();
void ExpandVariable();
void ExpandError();

void DoCaptureText();
void DoDup();
void DoError();
void DoExec();

Boolean CheckType();

char *SaveString();

Command *AllocCommand();
Command *FindCommand();
void AddCommand();
void DeleteCommand();
Resource *AllocResource();
InterfaceObject *AllocObject();
InterfaceObject *IndexObjectByName();
Shell *AllocShell();
Shell *IndexShell();
Shell *IndexShellByObject();
Shell *IndexShellByObjectName();
Environ *AllocEnviron();
Environ *IndexEnviron();
Environ *IndexEnvByShell();
Environ *IndexEnvByShellName();
Environ *IndexEnvByObject();
Environ *IndexEnvByObjectName();
Resource *IndexResource();
Boolean ShellInCurrentEnviron();
Boolean ShellHasPulldown();

void Create_Shell();
void CreateMenu();
void CreateMessageBoard();
void CreateCommandBoard();

Widget CreateObject();
Widget CreateButton();
Widget CreateLabel();
Widget CreateList();
Widget CreateMessage();
Widget CreateSeparator();
Widget CreateSlider();
Widget CreateTable();
Widget CreateTextEntry();
Widget CreateToggle();
Widget CreatePulldown();

void ClearListObject();
void ClearTable();
void ClearTablePart();
void ClearToggle();

void AddToggleInfo();
void DeleteToggleInfo();
ToggleData *IndexToggleData();
Boolean IsToggleSet();

void AddListInfo();
void DeleteListInfo();
ListData *IndexListData();
Boolean IsListSelected();

void AddCommandToControlBox();
void DeleteCommandFromControlBox();

Boolean parser_debug;
char *ParseCommand();

char *GetObjectValue();
char *strpart();
char *index();
char *rindex();
MessageInfo *ProcessMessage();

char *ResourceString();
int ResourceDataType();
unsigned int ResourceValid();
unsigned int ShellObjectValid();
char *ShellString();
char *ObjectString();

/* CallBacks */
void ShellDestroyedCB();
void ShellPopdownCB();
void ButtonPushCB();
 
void CommandPressedCB();

void helpCB();
void dismissCB();

void SSListChangedCB();
void MSListChangedCB();
void ESListChangedCB();
void BSListChangedCB();

void ToggleChangedCB();
void RadioChangedCB();

void saveCB();
void SaveOKCB();



/* Will supress "empty body for 'for' statement" violation message */
/* Will also supress for 'while' and 'if' statements */
/*SUPPRESS 530*/
