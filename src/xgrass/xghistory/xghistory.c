#include "xgrass_lib.h"
#include "Interact.h"
#include "History.h"
#include "gis.h"

struct {
  char progName[64];
  char mapName[128];
  char justMapName[512];
  char promptLabelString[512];
  struct History myHistory;
} Global;

Display                        *display;
Widget                          mainshell;

XtAppContext                    appContext;

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS History"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

void
Usage(s)
char *s;
{
  fprintf(stderr,"%s -help\n",s);
  fprintf(stderr,"%s mapname\n",s);
  fprintf(stderr,"Example:\n");
  fprintf(stderr,"\t%s geology\n",Global.progName);
}

char *
ParseCommand(argc,argv)
    /*ARGSUSED*/
    unsigned int argc;
    char **argv;
{
    strcpy(Global.progName,argv[0]);
    strcpy(Global.mapName,"");
    strcpy(Global.justMapName,"");
    strcpy(Global.promptLabelString,"Modify the history of your map");

    /* step past argv[0] */
    argv++;

    while (*argv) {
        if ( !strncmp(*argv,"-",1) || !strncmp(*argv,"+",1)) {
            Boolean match = False;

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
	     if (Global.mapName[0]) {
                fprintf(stderr,"%s:You may edit one history at a time\n",Global.progName,*argv);
                Usage(Global.progName);
		XFlush(display);
		exit(1);
	     }
	     else {
	       strcpy(Global.mapName,*argv);
	     }
        }
        argv++;
    }
    if (!Global.mapName[0]) {
      fprintf(stderr,"You must specify a map\n");
      Usage(Global.progName);
		XFlush(display);
	exit(1);
    }
}

void
OkCallback(w)
Widget w;
{
  G_write_history(Global.justMapName, &Global.myHistory);
		XFlush(display);
  exit(0);
}

void
CancelCallback(w)
Widget w;
{
		XFlush(display);
  exit(0);
}

void
DoHistoryDialog(argc,argv) 
int argc;
char **argv;
{
    Arg al[15];
    int ac = 0;
    Widget xgp;
    char mapset[512];

    ParseCommand(argc,argv);

    if (G__name_is_fully_qualified (Global.mapName,Global.justMapName,mapset)) {
        if (G__mapset_permissions(mapset) != 1) {
            fprintf(stderr, "You do not have write permission for mapset %s\n",mapset);
		XFlush(display);
            exit(1);
        }
    }
    else {
      strcpy(Global.justMapName,Global.mapName);
      strcpy(mapset,G_mapset());
    }

    if (G_read_history(Global.justMapName, mapset, &Global.myHistory)<0) {
        perror("Couldn't read history file");
		XFlush(display);
        exit(1);
    }

    XtSetArg(al[ac], XmNhistory, &Global.myHistory);
    ac++;

    XtSetArg(al[ac],XmNpromptLabelString,XmStringCreateSimple(Global.promptLabelString));ac++;

    XtSetArg(al[ac],XmNokLabelString,XmStringCreateSimple("Execute"));ac++;
    XtSetArg(al[ac],XmNcancelLabelString,XmStringCreateSimple("Cancel"));ac++;
    XtSetArg(al[ac],XmNenableWorkAreaStretch,True);ac++;
    xgp = XgCreateHistory(mainshell,"TESTIT",al,ac);
    XtManageChild(XgInteractorGetChild(xgp,XmINTERACT_PROMPT_LABEL));
    XtAddCallback(xgp,XmNcancelCallback,CancelCallback, NULL);
    XtAddCallback(xgp,XmNokCallback,OkCallback, NULL);
    XtManageChild(xgp);
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
					&argc, argv, NULL, NULL, 0);

    display = XtDisplay(shell);

    DoHistoryDialog(argc,argv);

    XtRealizeWidget(shell);
    XtAppMainLoop(appContext);

    return 0;
}
