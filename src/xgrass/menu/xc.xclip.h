/**********************************************************************
   xclip.h       - main header file
 *********************************************************************/
#include <xgrass_lib.h>
#include <Interact.h>
#include <Browser.h>

#include "hourglass.h"
#include "patchlevel.h"
#include "xc.types.h"

typedef struct _XclipGlobalData {
    char *scriptFile;       /* name of the input script file */
    char *progName;         /* program name */
    Widget applShell;       /* application shell widget (toplevel widget) */
    Widget interactor;      /* interactor widget */
    Widget help;            /* help shell widget */
    Boolean standAlone;
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
    _XC_data _xc_Data;
} XclipGlobalData;

#define SDC XmSTRING_DEFAULT_CHARSET

#ifdef MAIN
#ifdef Undefined
    XclipGlobalData *Global;
#endif
    char errorbuf[1024];
    Boolean commandDefaults;
    Boolean XCverbose;
#else  /* !MAIN */
#ifdef Undefined
    extern XclipGlobalData *Global;
#endif
    extern char errorbuf[1024];
    extern Boolean commandDefaults;
    extern Boolean XCverbose;
#endif /* MAIN */

struct Bunch {
  XclipGlobalData *theGlobal;
  void *data;
};
extern struct Bunch *CreateBunch();

extern int errno;

void XCInterrupt();
char *XCParseCommand();
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
char *_XcEnumStringToKey();
char *_XcEnumKeyToString();
