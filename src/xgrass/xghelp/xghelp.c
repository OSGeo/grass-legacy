#include "xgrass_lib.h"
#include "Interact.h"
#include "Help.h"
#include "gis.h"

struct {
  char progName[64];
  char promptLabelString[512];
  char title[512];
  char *helpfile;
} Global;

Display                        *display;
Widget                          mainshell;

XtAppContext                    appContext;

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS Help"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

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
    Global.helpfile = NULL;

    strcpy(Global.progName,argv[0]);
    strcpy(Global.promptLabelString,"XGrass Help");
    strcpy(Global.title,"GRASS Help");

    /* step past argv[0] */
    *argv++;

    while (*argv) {
        if ( !strncmp(*argv,"-",1) || !strncmp(*argv,"+",1)) {
            Boolean match = False;

            if ( !strncmp(*argv,"-label",6) ) {
		argv++;
		strcpy(Global.title, *argv);
		match = True;
            }
            if ( !strncmp(*argv,"-help",5) ) {
                Usage(Global.progName);
		XFlush(display);
		exit(0);
            }
            if ( !match ) {
                fprintf(stderr,"%s: unknown option \"%s\"\n",Global.progName,*argv);
                Usage(Global.progName);
		XFlush(display);
		exit(1);
            }
        } else {
	    Global.helpfile = *argv;
	}
        *argv++;
    }
}

void
OkCallback(w)
Widget w;
{
		XFlush(display);
  exit(0);
}

void
DoHelpDialog() 
{
    Arg al[15];
    int ac = 0;
    Widget xgh;
    char mapset[512];
    static XtCallbackRec cblist[] = {
	{ (XtCallbackProc)OkCallback, NULL},
	{ (XtCallbackProc)NULL, NULL},
    };

    if ( Global.helpfile == NULL ) {
	XtSetArg(al[ac], XmNhelpFile, "INDEX"); ac++;
    } else {
	XtSetArg(al[ac], XmNhelpFile, Global.helpfile); ac++;
    }
    XtSetArg(al[ac], XmNdismissOnly, True); ac++;
    XtSetArg(al[ac], XmNdestroyCallback, cblist); ac++;
    XtSetArg(al[ac], XmNpromptLabelString, XmStringCreateSimple(Global.promptLabelString)); ac++;
    XtSetArg(al[ac], XmNenableWorkAreaStretch,True);ac++;
    xgh = (Widget)XgCreateHelp(mainshell,"TESTIT",al,ac);
    /*XtManageChild(XgInteractorGetChild(xgh,XmINTERACT_PROMPT_LABEL));*/
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


    mainshell = shell = XtAppInitialize(&appContext, "XGrass",
					initTable, XtNumber(initTable),
					&argc, argv, NULL,NULL, 0);

    display = XtDisplay(shell);

    ParseCommand(argc,argv);

    DoHelpDialog();

    XtRealizeWidget(shell);
    XtAppMainLoop(appContext);

    return 0;
}
