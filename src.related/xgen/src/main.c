/**********************************************************************
   main.c       - Xgen main program
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
#include "xgen.h"


/*
 * Global Variables
 */

XgenGlobalData                  xgenGD;
char                            errorbuf[1024];

int
main(argc, argv)
    int                             argc;
    char                          **argv;
{
    char                           *cppflags = NULL;
    char                            cmd[80];
    extern FILE                    *yyin;
    Environ                        *e;
    int                             n;
    Atom                            protocol;


    /* start signal handling */
    signal(SIGKILL, XgenIntr);
    signal(SIGHUP, XgenIntr);
    signal(SIGINT, XgenIntr);
    signal(SIGQUIT, XgenIntr);

    /* set some global values */
    xgenGD.progName =
        (NULL != (xgenGD.progName = (char *) strrchr(argv[0], '/'))) ?
        ++xgenGD.progName : *argv;

    xgenGD.scriptFile = NULL;

    /* initialize the toolkit */
    XtToolkitInitialize();
#ifdef PRE_MOTIF_1_1
    /* and open the display (and a few other things...) */
    xgenGD.display = XtOpenDisplay(NULL, NULL, xgenGD.progName, "Xgen",
                                   NULL, 0, &argc, argv);
#else
    /* and open the display (and a few other things...) */
    xgenGD.appContext = XtCreateApplicationContext();
    xgenGD.display = XtOpenDisplay(xgenGD.appContext, NULL, xgenGD.progName,
                                   "Xgen", NULL, 0, &argc, argv);
#endif

    if (xgenGD.display == NULL) {
        sprintf(errorbuf, "%s:  Can't open display\n", xgenGD.progName);
        XgenFatalError("initializing Motif toolkit", errorbuf);
    }
    /* now the parse the rest of the command line */
    cppflags = ParseCommand(argc, argv);

    /* set lex input scriptFile to return from cpp or open the file */
    if (!nocpp) {
        if (!strcmp(xgenGD.scriptFile, "stdin")) {
            if (cppflags == NULL)
                sprintf(cmd, "/lib/cpp -P");
            else
                sprintf(cmd, "/lib/cpp -P %s", cppflags);
        } else {
            if (cppflags == NULL)
                sprintf(cmd, "/lib/cpp -P %s", xgenGD.scriptFile);
            else
                sprintf(cmd, "/lib/cpp -P %s %s", cppflags, xgenGD.scriptFile);
        }

        fflush(stdin);
        if ((yyin = popen(cmd, "r")) == NULL) {
            sprintf(errorbuf, "Can't open %s", xgenGD.scriptFile);
            XgenFatalError("running C preprocessor", errorbuf);
        }
    } else {
        if (!strcmp(xgenGD.scriptFile, "stdin"))
            yyin = stdin;
        else {
            yyin = fopen(xgenGD.scriptFile, "r");
        }
    }

    /* initialize the args used throughout the program */
    bzero((char *) args, sizeof(Arg) * MAXARGS);

    /* create the application shell widget */
    n = 0;
    SetGlobalArgs(&n, NOFONTS);
    SetObjectColorArgs(NULL, &n);
    XtSetArg(args[n], XmNx, 0);
    n++;
    XtSetArg(args[n], XmNy, 0);
    n++;
    XtSetArg(args[n], XmNallowShellResize, True);
    n++;
    if (nocontrol) {
        XtSetArg(args[n], XmNmappedWhenManaged, False);
        n++;
    }
    xgenGD.applShell = XtAppCreateShell(xgenGD.progName, "Xgen",
                      applicationShellWidgetClass, xgenGD.display, args, n);
    
    protocol = XmInternAtom(xgenGD.display,"WM_DELETE_WINDOW", False);
    XmAddWMProtocols(xgenGD.applShell,&protocol, 1);
    XtAddEventHandler(xgenGD.applShell, NoEventMask, True, XgenClientMessage, 
	NULL);

    /* parse the script, and get ptr to the internal representation */
    if (yyparse())
        XgenExit(1);
    if (parse_only)
        XgenExit(0);
    if (xgenGD.toplevelEnv == NULL)
        XgenFatalError("return from parser", "top level environment null");

    xgenGD.currentEnv = xgenGD.toplevelEnv;


    /* build the xgen control box */
    MakeControlBox();

    /* create the help and error widgets */
    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    xgenGD.help = XmCreateInformationDialog(xgenGD.applShell, "Help Dialog", args, n);
    xgenGD.error = XmCreateErrorDialog(xgenGD.applShell, "Error Dialog", args, n);
    {
        Widget                          temp;

        temp = XmMessageBoxGetChild(xgenGD.help, XmDIALOG_CANCEL_BUTTON);
        XtUnmanageChild(temp);
        temp = XmMessageBoxGetChild(xgenGD.help, XmDIALOG_HELP_BUTTON);
        XtUnmanageChild(temp);

        temp = XmMessageBoxGetChild(xgenGD.error, XmDIALOG_CANCEL_BUTTON);
        XtUnmanageChild(temp);
        temp = XmMessageBoxGetChild(xgenGD.error, XmDIALOG_HELP_BUTTON);
        XtUnmanageChild(temp);
    }

    e = xgenGD.toplevelEnv;
    while (e) {
        InitialShell(e);
        e = e->next;
    }

    if ( nocontrol ) 
	xgenGD.toplevelIsMapped = False;
    else 
	xgenGD.toplevelIsMapped = True;

    XtRealizeWidget(xgenGD.applShell);
    PopupEnviron(xgenGD.toplevelEnv);
    xgenGD.currentEnv = xgenGD.toplevelEnv;

#ifdef PRE_MOTIF_1_1
    XtMainLoop();
#else
    XtAppMainLoop(xgenGD.appContext);
#endif
    return (0);
}
