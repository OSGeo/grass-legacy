#define MAIN
#include "xc.xclip.h"

#define xclip_width 64
#define xclip_height 64

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)NULL},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

main(argc,argv)
    unsigned argc;
    char **argv;
{
    char *cppflags = NULL;
    char cmd[80];
    extern FILE *yyin;
    Widget shell;
    Atom protocol;
    Pixmap xclip_icon;
    Widget applShell;
    XtAppContext appContext;
    Display *display;

#ifdef USE_GRASS
    G_gisinit(argv[0]);
#endif /* USE_GRASS */

    /* initialize the toolkit  */
    /* and open the display (and a few other things...)  */
    
    applShell = XtAppInitialize(&appContext, "XClip", initTable, XtNumber(initTable), &argc, argv, NULL, NULL, 0);
    
    display = XtDisplay(applShell);
    shell = applShell;
    protocol = XmInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW", False);
    XmAddWMProtocols(shell, &protocol, 1);
    XtAddEventHandler(shell, NoEventMask, True, _XcWMClientMessage, shell);

    if ( XmIsMotifWMRunning(shell) ) {
        unsigned int decor_flags, func_flags;

        decor_flags = MWM_DECOR_BORDER | MWM_DECOR_RESIZEH;
        decor_flags |= MWM_DECOR_TITLE | MWM_DECOR_MENU;
        decor_flags |= MWM_DECOR_MINIMIZE;

        func_flags = MWM_FUNC_CLOSE | MWM_FUNC_RESIZE;
        func_flags |= MWM_FUNC_MOVE | MWM_FUNC_MINIMIZE;

        XtVaSetValues(shell,
            XmNmwmDecorations, decor_flags,
            XmNmwmFunctions, func_flags,
            NULL);
    }


    XClip(argc,argv,appContext,display,applShell,True);

    XtRealizeWidget(applShell);
    XtAppMainLoop (appContext);
}

/* UGLY!! This is not used bye xclip standalone, but it needs to be defined
 * or else it won't compile. This was done to overcome a problem created
 * because Gmake creates all targets for C source files to made into .o files
 */

__XgHistoryAddItem()
{
}
