#include "xgrass_lib.h"
#include "Interact.h"
#include "Region.h"
#include "gis.h"

struct {
  char progName[64];
  char gridColor[64];
  Boolean gridOn;
  Boolean editDefault;
  struct Cell_head myRegion;
  char promptLabelString[512];
} Global;

Display                        *display;
Widget                          mainshell;

XtAppContext                    appContext;

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS Region"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

void
Usage(s)
char *s;
{
  fprintf(stderr,"%s -help\n",s);
  fprintf(stderr,"%s [options]\n",s);
  fprintf(stderr,"\t-gridColor\t\tcolorname\n");
  fprintf(stderr,"\t-grid\t\t\t\tgrid OFF\n");
  fprintf(stderr,"\t+grid\t\t\t\tgrid ON\n");
  fprintf(stderr,"\t-promptLabelString\tstring\n");
  fprintf(stderr,"\tOne of:\n");
  fprintf(stderr,"\t\t-current\n");
  fprintf(stderr,"\t\t-default\n");
  fprintf(stderr,"Example:\n");
  fprintf(stderr,"\t%s -grid -current\n",Global.progName);
}

char *
ParseCommand(argc,argv)
    /*ARGSUSED*/
    unsigned int argc;
    char **argv;
{
    Global.editDefault = False;
    strcpy(Global.progName,argv[0]);
    strcpy(Global.gridColor,"");
    Global.gridOn = False;
    strcpy(Global.promptLabelString,"Modify your region");

    /* step past argv[0] */
    argv++;

    while (*argv) {
        if ( !strncmp(*argv,"-",1) || !strncmp(*argv,"+",1)) {
            Boolean match = False;

	    if (!strncmp(*argv,"-gridColor",10)) {
	      argv++;
	      strcpy(Global.gridColor,*argv);
	      match = True;
	    }
	    if (!strncmp(*argv,"-grid",5)) {
	      Global.gridOn = False;
	      match = True;
	    }
	    if (!strncmp(*argv,"+grid",5)) {
	      Global.gridOn = True;
	      match = True;
	    }
	    if (!strncmp(*argv,"-promptLabelString",17)) {
	      argv++;
	      strcpy(Global.promptLabelString,*argv);
	      match = True;
	    }
	    if (!strncmp(*argv,"-default",8)) {
	      Global.editDefault = True;
	      match = True;
	    }
	    if (!strncmp(*argv,"-current",8)) {
	      Global.editDefault = False;
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
             fprintf(stderr,"unknown option \"%s\"\n",*argv);
             Usage(Global.progName);
    XFlush(display);
	     exit(1);
        }
        argv++;
    }
}

void
OkCallback(w)
Widget w;
{
  char north[64];
  char south[64];
  char east[64];
  char west[64];
  char ns_res[64];
  char ew_res[64];
  char buf[512];

  if (Global.editDefault) {
    XBell(display,73);
  }
  else {
    if (G_put_window(&Global.myRegion)<0) {
      XBell(display,73);
    }
    G_format_northing(Global.myRegion.north, north, Global.myRegion.proj);
    G_format_northing(Global.myRegion.south, south, Global.myRegion.proj);
    G_format_easting(Global.myRegion.east, east, Global.myRegion.proj);
    G_format_easting(Global.myRegion.west, west, Global.myRegion.proj);
    G_format_resolution(Global.myRegion.ns_res, ns_res, Global.myRegion.proj);
    G_format_resolution(Global.myRegion.ew_res, ew_res, Global.myRegion.proj);

    sprintf(buf, "g.region n=%s s=%s e=%s w=%s nsres=%s ewres=%s",
	north,south,east,west,ns_res,ew_res);
    XgSetCommandString(display,XgGetMenuWindow(display),buf);
    XFlush(display);
  }
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
DoRegionDialog(argc,argv) 
int argc;
char **argv;
{
    Arg al[15];
    int ac = 0;
    Widget xgp;

    XtSetArg(al[ac],XmNgrid,Global.gridOn);ac++;

    if (Global.gridColor[0]) {
      XtSetArg(al[ac],XmNgridColor,Global.gridColor);ac++;
    }
    XtSetArg(al[ac],XmNeditDefaultRegion,Global.editDefault);ac++;
    if (Global.editDefault) {
      G_get_default_window(&Global.myRegion);
    }
    else {
      G_get_window(&Global.myRegion);
    }
    XtSetArg(al[ac],XmNcurrentRegion,&Global.myRegion);ac++;
    XtSetArg(al[ac],XmNpromptLabelString,XmStringCreateSimple(Global.promptLabelString));ac++;

    XtSetArg(al[ac],XmNokLabelString,XmStringCreateSimple("Execute"));ac++;
    XtSetArg(al[ac],XmNcancelLabelString,XmStringCreateSimple("Cancel"));ac++;
    XtSetArg(al[ac],XmNenableWorkAreaStretch,True);ac++;
    xgp = XgCreateRegion(mainshell,"Modify the Region",al,ac);
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

    ParseCommand(argc,argv);

    DoRegionDialog(argc,argv);

    XtRealizeWidget(shell);
    XtAppMainLoop(appContext);

    return 0;
}
