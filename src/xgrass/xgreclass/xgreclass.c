static char rcsid[] = "@(#)XGRASS $Id: xgreclass.c,v 0.0.0.1 1992/05/05 15:00:02 kurt Exp kurt $";
/*
 * File: xgreclass.c
 *
 * Desc: Top level program for reclassifying maps
 *
 * Auth: Eric W. Sink
 *
 * Date: 1 Feb 1992
 *
 * Modification History:
 *
 *
 */
#include "xgrass_lib.h"
#include "Interact.h"
#include "Reclass.h"
#include "gis.h"

struct {
  char progName[64];
  char mapName[64];
  char newMapName[64];
  char promptLabelString[512];
} Global;

Display                        *display;
Widget                          mainshell;

XtAppContext                    appContext;

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS Reclass"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

void
Usage(s)
char *s;
{
  fprintf(stderr,"%s -help\n",s);
  fprintf(stderr,"%s [options]\n",s);
  fprintf(stderr,"\t-map\t\tmapname\n");
  fprintf(stderr,"\t-newMap\t\tmapname\n");
  fprintf(stderr,"Example:\n");
  fprintf(stderr,"\t%s -map geology -newMap jeeology\n",Global.progName);
}

char *
ParseCommand(argc,argv)
    /*ARGSUSED*/
    unsigned int argc;
    char **argv;
{
    strcpy(Global.progName,argv[0]);
    strcpy(Global.mapName,"");
    strcpy(Global.newMapName,"");
    strcpy(Global.promptLabelString,"");

    /* step past argv[0] */
    argv++;

    while (*argv) {
        if ( !strncmp(*argv,"-",1) || !strncmp(*argv,"+",1)) {
            Boolean match = False;

	    if ( !strncmp(*argv,"-newMap",7) ) {
		argv++;
		strcpy(Global.newMapName,*argv);
		match = True;
            }
            if ( !strncmp(*argv,"-map",4) ) {
		argv++;
		strcpy(Global.mapName,*argv);
		match = True;
            }
            if ( !strncmp(*argv,"-help",5) ) {
                Usage(Global.progName);
		XFlush(display);
		exit(0);
            }
            if ( !match ) {
                fprintf(stderr,"unknown option \"%s\"\n",*argv);
                Usage(Global.progName);
		XFlush(display);
		exit(1);
            }
        } else {
	     if (Global.mapName[0]) {
	       strcpy(Global.newMapName,*argv);
	     }
	     else {
	       strcpy(Global.mapName,*argv);
	     }
        }
        argv++;
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
CancelCallback(w)
Widget w;
{
    XFlush(display);
    exit(0);
}

void
DoReclassDialog(argc,argv) 
int argc;
char **argv;
{
    Arg al[15];
    int ac = 0;
    Widget xgp;

    ParseCommand(argc,argv);

    if (Global.mapName[0]) {
    XtSetArg(al[ac], XmNoriginalMap, Global.mapName);
    ac++;
    }
    if (Global.newMapName[0]) {
    XtSetArg(al[ac], XmNnewMap, Global.newMapName);
    ac++;
    }

    XtSetArg(al[ac],XmNpromptLabelString,XmStringCreateSimple(Global.promptLabelString));ac++;

    XtSetArg(al[ac],XmNokLabelString,XmStringCreateSimple("Execute"));ac++;
    XtSetArg(al[ac],XmNcancelLabelString,XmStringCreateSimple("Cancel"));ac++;
    XtSetArg(al[ac],XmNenableWorkAreaStretch,True);ac++;
    XtSetArg(al[ac],XmNreclassOnOk,True);ac++;
    xgp = XgCreateReclass(mainshell,"Reclass Data in a Raster Map Layer",al,ac);
    XtManageChild(XgInteractorGetChild(xgp,XmINTERACT_PROMPT_LABEL));
    XtAddCallback(xgp,XmNcancelCallback,CancelCallback, NULL);
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

    DoReclassDialog(argc,argv);

    XtRealizeWidget(shell);
    XtAppMainLoop(appContext);

    return 0;
}
