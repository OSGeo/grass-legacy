#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
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
#include <Xm/Separator.h>
#include "widgets/Table.h"
#include <Xm/Text.h>
#include <Xm/ToggleB.h>

#include "types.h"

#define MAXARGS 100
Arg args[MAXARGS];


typedef struct _XgenGlobalData {
	char *scriptFile;     /* name of the input script file */
	char *progName;       /* program name */
	Command *commandList; /* list of commands still active */
	ToggleInfo *toggleInfo;/* pointer to info on active toggles */
	ListInfo *listInfo;   /* pointer to info on active lists */
	Environ *toplevelEnv; /* pointer to the application data */
	Environ *currentEnv;  /* pointer to the currently displayed environment */
	Widget applShell;     /* application shell widget (toplevel widget) */
	Widget CBRowCol;      /* control box row column widget */
	Widget help;          /* help shell widget */
	Widget error;         /* error shell widget */
	Display *display;     /* the display */
	int screen;           /* screen info: for X calls */
	Screen *scrptr;       /* for Xt and Xm calls, sigh */
	Colormap cmap;        /* default colormap */	
	XmFontList g_fixed;   /* the "fixed" font info */
	char *g_font;         /* font */
	XFontStruct *g_fs;
	char *g_ffont;        /* fixed font */
	XFontStruct *g_ffs;
	char *g_bg;           /* background color */
	XColor g_bgs;
	char *g_fg;           /* foreground color */
	XColor g_fgs;
	char *g_bgpix;        /* background pixmap */
	Pixmap g_bgpm;
	char *g_ts;           /* top shadow color */
	XColor g_tss;
	char *g_tspix;        /* top shadow pixmap */
	Pixmap g_tspm;
	char *g_bs;           /* bottom shadow color */
	XColor g_bss;
	char *g_bspix;        /* bottom shadow pixmap */
	Pixmap g_bspm;
} XgenGlobalData;

#define NOFONTS 0
#define FONTS   1

#define SDC XmSTRING_DEFAULT_CHARSET

extern XgenGlobalData xgenGD;
extern char errorbuf[1024];
extern Boolean verbose;
extern Boolean check;
extern Boolean parse_only;
extern Boolean nocpp;

extern int errno;
void XgenFatalError();
void XgenWarning();

void XgenIntr();
void XgenExit();

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

void ClearTable();
void ClearTablePart();
void ClearToggle();

void AddToggleInfo();
ToggleData *IndexToggleData();
Boolean IsToggleSet();

void AddListInfo();
ListData *IndexListData();
Boolean IsListSelected();

void AddCommandToControlBox();
void DeleteCommandFromControlBox();

Boolean parser_debug;

char *GetObjectValue();
char *strpart();
MessageInfo *ProcessMessage();

char *ResourceString();
int ResourceDataType();
unsigned int ResourceValid();

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




