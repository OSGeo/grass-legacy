#include "xgrass_lib.h"
#include "Interact.h"
#include "Browser.h"

struct {
  char progName[64];
  int numLists;
  Boolean list1IsStatic;
  Boolean list2IsStatic;
  Boolean list3IsStatic;
  int browseMode;
  char *db_element;
  int selMode;
  char promptLabelString[512];
  char initialMapset1[256];
  char initialMapset2[256];
  char initialMapset3[256];
} Global;

Display                        *display;
Widget                          mainshell;

XtAppContext                    appContext;

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS Browser"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

void
Usage(s)
char *s;
{
  fprintf(stderr,"%s -help\n",s);
  fprintf(stderr,"%s [options]\n",s);
  fprintf(stderr,"\t-numLists\t\tn\t(1, 2 or 3)\n");
  fprintf(stderr,"\t-list[123]IsStatic\t\tstatic OFF\n");
  fprintf(stderr,"\t+list[123]IsStatic\t\tstatic ON\n");
  fprintf(stderr,"\t-initialMapset[123]\tname\n");
  fprintf(stderr,"\tOne of:\n");
  fprintf(stderr,"\t\t-raster\n");
  fprintf(stderr,"\t\t-vector\n");
  fprintf(stderr,"\t\t-site\n");
  fprintf(stderr,"\t\t-region\n");
  fprintf(stderr,"\t\t-icon\n");
  fprintf(stderr,"\t\t-label\n");
  fprintf(stderr,"\t\t-group\n");
  fprintf(stderr,"\t\t-asciidlg\n");
  fprintf(stderr,"\t\t-dlg\n");
  fprintf(stderr,"\t\t-asciivector\n");
  fprintf(stderr,"\t\t-segment\n");
  fprintf(stderr,"\t\t-user_defined db_element\n");
  fprintf(stderr,"\tOne of:\n");
  fprintf(stderr,"\t\t-single\n");
  fprintf(stderr,"\t\t-multiple\n");
  fprintf(stderr,"\t-promptLabelString\tprompt\n");
  fprintf(stderr,"Example:\n");
  fprintf(stderr,"\t%s -numLists 1 -list1IsStatic -initialMapset1 %s -raster\n",Global.progName,_XgGetUserName());
}

char *
ParseCommand(argc,argv)
    /*ARGSUSED*/
    unsigned int argc;
    char **argv;
{
    strcpy(Global.progName,argv[0]);
    Global.numLists = 1;
    Global.list1IsStatic = False;
    Global.list2IsStatic = False;
    Global.list3IsStatic = False;
    Global.browseMode = XG_RASTER;
    Global.selMode = XG_SINGLE_SELECT;
    strcpy(Global.initialMapset1,"");
    strcpy(Global.initialMapset2,"");
    strcpy(Global.initialMapset3,"");
    strcpy(Global.promptLabelString,"Please select a raster map.");

    /* step past argv[0] */
    argv++;

    while (*argv) {
        if ( !strncmp(*argv,"-",1) || !strncmp(*argv,"+",1)) {
            Boolean match = False;

	    if (!strncmp(*argv,"-numLists",9)) {
	      argv++;
	      Global.numLists = atoi(*argv);
	      if (Global.numLists > 3) {
		fprintf(stderr,"You may not have more than 3 lists\n");
		Usage(Global.progName);
		XFlush(display);
		exit(1);
	      }
	      match = True;
	    }
	    if (!strncmp(*argv,"-promptLabelString",17)) {
	      argv++;
	      strcpy(Global.promptLabelString,*argv);
	      match = True;
	    }
	    if (!strncmp(*argv,"-list1IsStatic",14)) {
	      Global.list1IsStatic = False;
	      match = True;
	    }
	    if (!strncmp(*argv,"+list1IsStatic",14)) {
	      Global.list1IsStatic = True;
	      match = True;
	    }
	    if (!strncmp(*argv,"-list2IsStatic",14)) {
	      Global.list2IsStatic = False;
	      match = True;
	    }
	    if (!strncmp(*argv,"+list2IsStatic",14)) {
	      Global.list2IsStatic = True;
	      match = True;
	    }
	    if (!strncmp(*argv,"-list3IsStatic",14)) {
	      Global.list3IsStatic = False;
	      match = True;
	    }
	    if (!strncmp(*argv,"+list3IsStatic",14)) {
	      Global.list3IsStatic = True;
	      match = True;
	    }
	    if (!strncmp(*argv,"-initialMapset1",15)) {
	      argv++;
	      strcpy(Global.initialMapset1,*argv);
	      match = True;
	    }
	    if (!strncmp(*argv,"-initialMapset2",15)) {
	      argv++;
	      strcpy(Global.initialMapset2,*argv);
	      match = True;
	    }
	    if (!strncmp(*argv,"-initialMapset3",15)) {
	      argv++;
	      strcpy(Global.initialMapset3,*argv);
	      match = True;
	    }
	    if (!strncmp(*argv,"-raster",7)) {
	      Global.browseMode = XG_RASTER;
	      match = True;
	    }
	    if (!strncmp(*argv,"-vector",7)) {
	      Global.browseMode = XG_VECTOR;
	      match = True;
	    }
	    if (!strncmp(*argv,"-site",5)) {
	      Global.browseMode = XG_SITE;
	      match = True;
	    }
	    if (!strncmp(*argv,"-region",7)) {
	      Global.browseMode = XG_REGION;
	      match = True;
	    }
	    if (!strncmp(*argv,"-icon",5)) {
	      Global.browseMode = XG_ICON;
	      match = True;
	    }
	    if (!strncmp(*argv,"-label",6)) {
	      Global.browseMode = XG_LABEL;
	      match = True;
	    }
	    if (!strncmp(*argv,"-group",6)) {
	      Global.browseMode = XG_GROUP;
	      match = True;
	    }
	    if (!strncmp(*argv,"-asciidlg",9)) {
	      Global.browseMode = XG_ASCII_DLG;
	      match = True;
	    }
	    if (!strncmp(*argv,"-dlg",4)) {
	      Global.browseMode = XG_DLG;
	      match = True;
	    }
	    if (!strncmp(*argv,"-asciivector",12)) {
	      Global.browseMode = XG_ASCII_VECTOR;
	      match = True;
	    }
	    if (!strncmp(*argv,"-segment",8)) {
	      Global.browseMode = XG_SEGMENT;
	      match = True;
	    }
	    if (!strncmp(*argv,"-user_defined",13)) {
	      argv++;
	      Global.browseMode = XG_USER_DEFINED;
	      Global.db_element = XtNewString(*argv);
	      match = True;
            }
	    if (!strncmp(*argv,"-single",7)) {
	      Global.selMode = XG_SINGLE_SELECT;
	      match = True;
	    }
	    if (!strncmp(*argv,"-multiple",9)) {
	      Global.selMode = XG_MULTIPLE_SELECT;
	      match = True;
	    }
            if ( !strncmp(*argv,"-help",5) ) {
		match = True;
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
  char *s;
  XmString result;
  XtVaGetValues(w,XmNresultString,&result,NULL);
  XmStringGetLtoR(result,XmSTRING_DEFAULT_CHARSET,&s);
  printf("%s\n",s);
  XFlush(display);
  exit(0);
}

void
CancelCallback(w)
Widget w;
{
  printf("\n");
  XFlush(display);
  exit(0);
}

void
DoBrowserDialog(argc,argv) 
int argc;
char **argv;
{
    Arg al[15];
    int ac = 0;
    Widget xgp;

    ParseCommand(argc,argv);

    XtSetArg(al[ac],XmNnumLists,Global.numLists);ac++;

    XtSetArg(al[ac],XmNpromptLabelString,XmStringCreateSimple(Global.promptLabelString));ac++;
    XtSetArg(al[ac],XmNselMode,Global.selMode);ac++;
    XtSetArg(al[ac],XmNbrowseMode,Global.browseMode);ac++;
    if ( Global.browseMode == XG_USER_DEFINED ) {
	XtSetArg(al[ac],XmNuserDBElement,XmStringCreateSimple(Global.db_element));ac++;
    }
    if (Global.numLists>=1) {
      XtSetArg(al[ac],XmNlist1IsStatic,Global.list1IsStatic);ac++;
      if (*Global.initialMapset1) {
          XtSetArg(al[ac],XmNinitialMapset1,XmStringCreateSimple(Global.initialMapset1));ac++;
	  }
    }
    if (Global.numLists>=2) {
      XtSetArg(al[ac],XmNlist2IsStatic,Global.list2IsStatic);ac++;
      if (*Global.initialMapset2) {
          XtSetArg(al[ac],XmNinitialMapset2,XmStringCreateSimple(Global.initialMapset2));ac++;
	  }
    }
    if (Global.numLists>=3) {
      XtSetArg(al[ac],XmNlist3IsStatic,Global.list3IsStatic);ac++;
      if (*Global.initialMapset3) {
          XtSetArg(al[ac],XmNinitialMapset3,XmStringCreateSimple(Global.initialMapset3));ac++;
	  }
    }

    XtSetArg(al[ac],XmNokLabelString,XmStringCreateSimple("Done"));ac++;
    XtSetArg(al[ac],XmNcancelLabelString,XmStringCreateSimple("Cancel"));ac++;
    XtSetArg(al[ac],XmNenableWorkAreaStretch,True);ac++;
    XtSetArg(al[ac],XmNlistAllMapsets, False); ac++;
    xgp = XgCreateBrowser(mainshell,"TESTIT",al,ac);
    XtManageChild(XgInteractorGetChild(xgp,XmINTERACT_PROMPT_LABEL));
    XtAddCallback(xgp,XmNcancelCallback,CancelCallback, NULL);
    XtAddCallback(xgp,XmNokCallback,OkCallback, NULL);
    XtManageChild(xgp);
}

int
main(argc, argv)
    unsigned                        argc;
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

    DoBrowserDialog(argc,argv);

    XtRealizeWidget(shell);
    XtAppMainLoop(appContext);

    return 0;
}
