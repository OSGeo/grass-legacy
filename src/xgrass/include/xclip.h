/**********************************************************************
   xclip.h       - main header file
 *********************************************************************/
#include <xgrass_lib.h>
#include <Interact.h>
#include <Browser.h>

#include "hourglass.h"
#include "patchlevel.h"
#include "xclip_types.h"

typedef struct _XclipGlobalData {
    char *scriptFile;       /* name of the input script file */
    char *progName;         /* program name */
    Widget applShell;       /* application shell widget (toplevel widget) */
    Widget interactor;      /* interactor widget */
    Widget help;            /* help shell widget */
    Widget error;           /* error shell widget */
    Display *display;       /* the display */
    int screen;             /* screen info: for X calls */
    Screen *scrptr;         /* for Xt and Xm calls, sigh */
    Colormap cmap;          /* default colormap */    
    XtAppContext appContext;/* the application context */
    Boolean captureSet;     /* capture set from command line (override script) */
    Boolean capture;        /* should we capture or not */
#ifdef USE_GRASS
    Boolean needGRASS;      /* we have some GRASS data elements */
#endif /* USE_GRASS */
} XclipGlobalData;

#define SDC XmSTRING_DEFAULT_CHARSET

#ifdef MAIN
    XclipGlobalData Global;
    char errorbuf[1024];
    Boolean commandDefaults;
    Boolean verbose;
#else  /* !MAIN */
    extern XclipGlobalData Global;
    extern char errorbuf[1024];
    extern Boolean commandDefaults;
    extern Boolean verbose;
#endif /* MAIN */

extern int errno;

void Interrupt();
char *ParseCommand();
void _XcWMClientMessage();
Boolean _XcUniqueName(), _XcIsName();
XCInterfaceData *_XcGetFlagByFlagData();
XCInterfaceData *_XcGetParmByParmData();
XCInterfaceData *_XcGetFlagByName();
XCInterfaceData *_XcGetParmByName();
XCRequireData *_XcReturnAppropriatePrecludeData();
XCRequireData *_XcReturnAppropriateRequireData();
char *_XcGetStringValueByName();
char *_XcBuildCommandString();
Boolean _XcCheckRequirements();
Boolean _XcCheckOptional();
