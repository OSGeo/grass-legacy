#include "xgen.h"

/*
 * Global Varaibles
 */

XgenGlobalData xgenGD;

main(argc,argv) 
int argc;
char **argv;
{
	char *rindex(); 
	char cmd[80];
	int fi;
	extern FILE *yyin;
	Environ *e;
	int n;


	/* start signal handling */
	signal(SIGKILL,XgenIntr);
	signal(SIGHUP,XgenIntr);
	signal(SIGINT,XgenIntr);
	signal(SIGQUIT,XgenIntr);

	/* set some global values */
	xgenGD.progName = (xgenGD.progName=rindex(argv[0],'/'))?++xgenGD.progName:*argv;
	xgenGD.scriptFile = NULL;

	/* initialize the toolkit */
	XtToolkitInitialize();
	/* open the display (and a few other things...) */
	xgenGD.display = XtOpenDisplay(NULL,NULL,xgenGD.progName,"Xgen",
							 	NULL,0,&argc,argv);

	if (xgenGD.display == NULL) {
		char errorbuf[80];

		sprintf(errorbuf, "%s:  Can't open display\n", xgenGD.progName);
		XgenFatalError("initializing Motif toolkit",errorbuf);
	}

	/* now the parse the rest of the command line */
	ParseCommand(argc,argv);
	
	/* set lex input scriptFile to return from cpp or open the file */
	if ( ! nocpp ) {
		if ( !strcmp(xgenGD.scriptFile,"stdin") )  {
			sprintf(cmd,"/lib/cpp -P");
		} else { 
			sprintf(cmd,"/lib/cpp -P %s",xgenGD.scriptFile);
		}
	
		if ((yyin = popen(cmd,"r")) == NULL ) {
			char errorbuf[80];
			sprintf(errorbuf,"Can't open %s",xgenGD.scriptFile);
			XgenFatalError("running C preprocessor",errorbuf);
		}
	} else {
		if ( !strcmp(xgenGD.scriptFile,"stdin") ) 
			yyin = stdin;
		else {
			yyin = fopen(xgenGD.scriptFile,"r");
		}
	}

	/* create the application shell widget */
	n = 0;
	/* KAB - -geo should define this if applicable */
	SetGlobalArgs(&n,NOFONTS);
	SetObjectColorArgs(NULL,&n);
	XtSetArg(args[n],XmNx,0);n++;
	XtSetArg(args[n],XmNy,0);n++;
	XtSetArg(args[n],XmNoverrideRedirect,(XtArgVal)True);n++;
	XtSetArg(args[n],XmNallowShellResize,(XtArgVal)True);n++;
	xgenGD.applShell = XtAppCreateShell(xgenGD.progName,NULL,
						applicationShellWidgetClass,xgenGD.display,args,n);

	/* parse the script, and get ptr to the internal representation */
	if ( yyparse() ) XgenExit(1);
	if ( parse_only ) XgenExit(0);
	if ( xgenGD.toplevelEnv == NULL ) 
		XgenFatalError("return from parser","top level environment null");

	/* build the xgen control box */
	MakeControlBox();

	/*create the help and error widgets */
	/*KAB change the help widget to be more helpful... */
	n = 0;
	SetGlobalArgs(&n,FONTS);
	SetObjectColorArgs(NULL,&n);
	xgenGD.help = XmCreateInformationDialog(xgenGD.applShell,"Help Dialog",args,n);
	xgenGD.error = XmCreateErrorDialog(xgenGD.applShell,"Error Dialog",args,n);

	e = xgenGD.toplevelEnv;
	while ( e ) {
		InitialShell(e);
		e = e->next;
	}

	XtRealizeWidget(xgenGD.applShell);
	PopupEnviron(xgenGD.toplevelEnv);
	xgenGD.currentEnv = xgenGD.toplevelEnv;

	XtMainLoop();
	

}
