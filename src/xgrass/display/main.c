#define MAIN
#include "xgdisp.h"

static XrmOptionDescRec optTable[] = {
{"-pagewidth",	".pageWidth",	XrmoptionSepArg,	(caddr_t)"8.5"},
{"-pw",		".pageWidth",	XrmoptionSepArg,	(caddr_t)"8.5"},
{"-pageheight",	".pageHeight",	XrmoptionSepArg,	(caddr_t)"11.0"},
{"-ph",		".pageHeight",	XrmoptionSepArg,	(caddr_t)"11.0"},
{"-units",	".units",	XrmoptionSepArg,	(caddr_t)NULL},
{"-rulerfont",	".rulerFont",	XrmoptionSepArg,	(caddr_t)"fixed"},
{"-visual",	".visual",	XrmoptionSepArg,	(caddr_t)NULL},
{"-help",	".help",	XrmoptionNoArg,		(caddr_t)"on"},
};

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS Display"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

void 
#ifdef _NO_PROTO
Usage(prog)
char *prog;
#else
Usage(char *prog)
#endif
{
    fprintf(stderr,"usage:\n\t%s [-options ...] [file]\n\n", prog);
    fprintf(stderr,"where options include:\n");
    fprintf(stderr,"\t-help\t\t\tprint out this message\n");
    fprintf(stderr,"\t-fg color\t\t\tinterface foreground color\n");
    fprintf(stderr,"\t-bg color\t\t\tinterface background color\n");
    fprintf(stderr,"\t-fn fontname\t\t\tinterface font\n");
    fprintf(stderr,"\t-title string\t\t\ttitle string\n");
    exit(0);
}

yyerror(s)
char *s;
{
    extern int yylineno;
    extern int yyerrflag;
    extern char yytext[];

    fprintf(stderr,"\n\"%s\", line %d:\n<%s>", yyfilename,
        yylineno,s);

    fprintf(stderr," at or near \"%s\"\n",yytext);

    yyerrflag++;
}


yywarning(s)
char *s;
{
    extern int yylineno;
    extern char yytext[];

    fprintf(stderr,"\n\"%s\", line %d:\n<%s>", yyfilename,
        yylineno,s);

    fprintf(stderr," at or near \"%s\"\n",yytext);
}

#ifdef _NO_PROTO
main(argc,argv)
	unsigned argc;
	char **argv;
#else
main(unsigned argc, char **argv)
#endif
{
    Widget shell;
    XrmValue value;
    XrmDatabase db = NULL;
    char buf[256];
    XVisualInfo vinfo;
    XVisualInfo *vPtr;
    Atom protocol;

    yyfilename = NULL;
    G_gisinit(argv[0]);

    XtToolkitInitialize();
    Global.appContext = XtCreateApplicationContext();
    Global.display = XtOpenDisplay(Global.appContext, NULL, NULL, "XGrass", 
        initTable, XtNumber(initTable), &argc, argv);

    if (Global.display == NULL) {
        fprintf(stderr, "Error: Can't open display.\n");
        exit(1);
    }

    
    Global.hRuler = (Pixmap)NULL;
    Global.vRuler = (Pixmap)NULL;
    Global.mode = XGD_MODE_SELECT;
    Global.rulerFontStruct = NULL;
    Global.screenNo = DefaultScreen(Global.display);
    Global.visual = NULL;
    Global.visualClass = 99;
    Global.currentObject = NULL;
    Global.selectedObjects = NULL;
    Global.objectList = NULL;

    /* Parse the rest of the command line */
    XrmParseCommand(&db, optTable, XtNumber(optTable), argv[0], &argc, argv);

    if ( XrmGetResource(db, "xgdisplay.help", "XGrass.help", buf, 
               &value) == True ) {
        Usage(argv[0]);
    }
    /* Get the visual */
    if ( XrmGetResource(db, "xgdisplay.visual", "XGrass.visual", buf,
               &value) == True ) {
        printf("visual = %s\n",value.addr);
	if ( !strcmp("StaticGray", value.addr) ) {
            Global.visualClass = StaticGray;
	} else if ( !strcmp("GrayScale", value.addr) ) {
            Global.visualClass = GrayScale;
	} else if ( !strcmp("StaticColor", value.addr) ) {
            Global.visualClass = StaticColor;
	} else if ( !strcmp("PseudoColor", value.addr) ) {
            Global.visualClass = PseudoColor;
	} else if ( !strcmp("TrueColor", value.addr) ) {
            Global.visualClass = TrueColor;
	} else if ( !strcmp("DirectColor", value.addr) ) {
            Global.visualClass = DirectColor;
	} else {
            fprintf(stderr, "Warning: Invalid visual class [%s], ", value.addr);
            fprintf(stderr, "using default.\n");
	}
    }
    if ( Global.visualClass != 99 ) {
        int num;

        vinfo.class = Global.visualClass;
        vPtr = XGetVisualInfo(Global.display, VisualClassMask, &vinfo, &num);
        if ( num == 0 ) {
            fprintf(stderr, "Warning: This hardware does not support ");
            fprintf(stderr, "the %s visual, using default\n",value.addr);
            Global.visualClass = 99;
        } else {
            Global.visual = vPtr[0].visual;
            Global.depth = vPtr[0].depth;
        }
    } 
    /* nothing yet...use the default */
    if ( Global.visualClass == 99 ) {
        int num;

	Global.visual = DefaultVisual(Global.display, Global.screenNo);
	Global.visualClass = Global.visual->class;
	Global.depth = DefaultDepth(Global.display, Global.screenNo);
        vinfo.class = Global.visualClass;
        vPtr = XGetVisualInfo(Global.display, VisualClassMask, &vinfo, &num);
    }

    /* create top level shell */
    Global.applShell = shell = XtVaAppCreateShell(argv[0], "XGrass", 
        applicationShellWidgetClass, Global.display, 
        XtNscreen, DefaultScreenOfDisplay(Global.display),
        NULL);

    protocol = XmInternAtom(XtDisplay(Global.applShell), "WM_DELETE_WINDOW", False);
    XmAddWMProtocols(Global.applShell, &protocol, 1);
    XtAddEventHandler(Global.applShell, NoEventMask, True, _XgWMClientMessage, Global.applShell);

    /* get the colormap  */
    Global.cmap = DefaultColormap(Global.display, Global.screenNo);
    
    /* get the screen pointer */
    Global.screenPtr = XtScreen(shell);

    /* Get the page width and height */
    if ( XrmGetResource(db, "xgdisplay.pageWidth", "XGrass.pageWidth", buf,
               &value) == True ) {
        printf("page width = %s\n",value.addr);
        if (sscanf(value.addr,"%lf", &Global.pageWidth) != 1 ) {
            fprintf(stderr, "Page width must be a decimal number\n");
            exit(1);
        }
    } else {
	Global.pageWidth = 8.5;
    }
    if ( XrmGetResource(db, "xgdisplay.pageHeight", "XGrass.pageHeight", buf,
               &value) == True ) {
        printf("page height = %s\n",value.addr);
        if (sscanf(value.addr,"%lf", &Global.pageHeight) != 1 ) {
            fprintf(stderr, "Page height must be a decimal number\n");
            exit(1);
        }
    } else {
	Global.pageHeight = 11.0;
    }
    /* set the ruler font */
    if ( XrmGetResource(db, "xgdisplay.rulerFont", "XGrass.rulerFont", buf,
               &value) == True ) {
        printf("rulerFont = %s\n",value.addr);
        if ( (Global.rulerFontStruct = 
                  XLoadQueryFont(Global.display, value.addr)) == NULL) {
            fprintf(stderr,"Warning: Ruler font [%s] not found\n", value.addr);
        }
    } 
    if ( Global.rulerFontStruct == NULL ) {
        if ( (Global.rulerFontStruct = 
                  XLoadQueryFont(Global.display, "fixed")) == NULL) {
            fprintf(stderr,"Warning: Default ruler font [fixed] not found\n");
            exit(1);
        }
    }
    if ( (Global.fontStruct = 
	      XLoadQueryFont(Global.display, "fixed")) == NULL) {
	fprintf(stderr,"Warning: Default font [fixed] not found\n");
	exit(1);
    }
    Global.fontName = XtNewString("fixed");
    /* set the ruler and readout units */
    if ( XrmGetResource(db, "xgdisplay.units", "XGrass.units", buf,
               &value) == True ) {
        printf("units = %s\n",value.addr);
        if ( !strncmp(value.addr,"i",2) ) {
	    Global.units = XGD_UNITS_INCHES;
        } else if ( !strncmp(value.addr,"m",1) ) {
	    Global.units = XGD_UNITS_MILLI;
        } else if ( !strncmp(value.addr,"p",1) ) {
	    Global.units = XGD_UNITS_PIXELS;
        } else {
            fprintf(stderr, 
          "Units must be inches [i], millimeters [m], or pixels [p]\n");
            exit(1);
        }
    } else {
	Global.units = XGD_UNITS_PIXELS;
    }

    CreateLayout(shell);

    XtRealizeWidget(shell);

    XgdInit(Global.display, Global.cmap, vPtr, False);

    /* reserve highlight color */
    Global.highlight = XgdGetHighlightColor();

    /* reserve vector colors */
    Global.vectColors = 
        XgdInitVectorColors(Global.display,&Global.numVectColors);

    if ( Global.numVectColors == 0 ) {
        fprintf(stderr, "Error: Couldn't allocate vector colors.\n");
    }

    Global.fillPattern = XGD_FILL_PATTERN_NONE;
    Global.linePattern = XGD_LINE_PATTERN_SOLID;
    Global.lineWidth = 1;
    Global.foreground = XgdGetVectColorPixelByName("black");
    Global.background = XgdGetVectColorPixelByName("white");

    XtAppMainLoop(Global.appContext);

    return 0;
}
