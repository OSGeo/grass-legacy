#include "xgrass_lib.h"
#include "Interact.h"
#include "Help.h"
#include "gis.h"

struct {
  char progName[64];
  char promptLabelString[512];
} Global;

Display                        *display;
Widget                          mainshell;

XtAppContext                    appContext;

void
Usage(s)
char *s;
{
  fprintf(stderr,"%s -help\n",s);
  fprintf(stderr,"%s\n",s);
}

char *
ParseCommand(argc,argv)
    /*ARGSUSED*/
    unsigned int argc;
    char **argv;
{
    strcpy(Global.progName,argv[0]);
    strcpy(Global.promptLabelString,"XGrass Help");

    /* step past argv[0] */
    *argv++;

    while (*argv) {
        if ( !strncmp(*argv,"-",1) || !strncmp(*argv,"+",1)) {
            Boolean match = False;

            if ( !strncmp(*argv,"-help",5) ) {
                Usage(Global.progName);
		exit(0);
            }
            if ( !match ) {
                fprintf(stderr,"%s: unknown option \"%s\"\n",Global.progName,*argv);
                Usage(Global.progName);
		exit(1);
            }
        } else {
        }
        *argv++;
    }
}

void
OkCallback(w)
Widget w;
{
  exit(0);
}

void
CancelCallback(w)
Widget w;
{
  exit(0);
}

void
DoHelpDialog(argc,argv) 
int argc;
char **argv;
{
    Arg al[15];
    int ac = 0;
    Widget xgh;
    char mapset[512];

    ParseCommand(argc,argv);

    XtSetArg(al[ac], XmNhelpFile, "INDEX"); ac++;
    XtSetArg(al[ac], XmNdismissOnly, True); ac++;
    XtSetArg(al[ac], XmNokLabelString, XmStringCreateSimple("Done")); ac++;
    XtSetArg(al[ac], XmNpromptLabelString, XmStringCreateSimple(Global.promptLabelString)); ac++;
    XtSetArg(al[ac], XmNautoUnmanage, True); ac++;
    XtSetArg(al[ac], XmNenableWorkAreaStretch,True);ac++;
    xgh = XgCreateHelp(mainshell,"TESTIT",al,ac);
    XtManageChild(XgInteractorGetChild(xgh,XmINTERACT_PROMPT_LABEL));
    XtAddCallback(xgh,XmNcancelCallback,CancelCallback, NULL);
    XtAddCallback(xgh,XmNokCallback,OkCallback, NULL);
    XtUnmanageChild(XgInteractorGetChild(xgh,XmINTERACT_CANCEL_BUTTON));
    XtUnmanageChild(XgInteractorGetChild(xgh,XmINTERACT_HELP_BUTTON));

    XtManageChild(xgh);
}

int
main(argc, argv)
    unsigned int                    argc;
    char                          **argv;
{
    Widget                          shell;

    /* initialize the toolkit  */
    /* and open the display (and a few other things...)  */
    G_gisinit(argv[0]);
    XtToolkitInitialize();
    appContext = XtCreateApplicationContext();
    display = XtOpenDisplay(appContext, NULL, argv[0],
                            "XGrass", NULL, 0, &argc, argv);

    if (display == NULL) {
        fprintf(stderr, "%s:  Can't open display\n", argv[0]);
	exit(1);
    }
    mainshell = shell = XtVaAppCreateShell("XgHelp", "XGrass", applicationShellWidgetClass, display,
                                           XmNmappedWhenManaged, True,
                                           NULL);

    DoHelpDialog(argc,argv);

    XtRealizeWidget(shell);
    XtAppMainLoop(appContext);

    return 0;
}
