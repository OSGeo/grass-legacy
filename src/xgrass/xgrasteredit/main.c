
/*
 * FILE: main.c
 *
 * PROGRAMMER: David M. Johnson
 *
 * This is the main function for the XGRASS raster-editor, which
 * is an X/Motif based interface for editing GRASS-GIS raster map 
 * layers. 
 *
 * This function initializes the GRASS libraries, the XGRASS 
 * display library, the XGRASS interface/widget library, various 
 * globals, and it calls CreateLayout() (in layout.c) to create 
 * and layout the widgets that make up the main window of the 
 * interface.
 *
 */

#define MAIN
#include "xgre.h"
#include "bmaps.h"

static XrmOptionDescRec optTable[] = {
{"-visual",	".visual",	XrmoptionSepArg,	(caddr_t)NULL},
};

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS CellEditor"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

/************/
/*** main ***/
/************/

#ifdef _NO_PROTO
main(argc,argv)
	unsigned argc;
	char **argv;
#else
main(unsigned argc, char **argv)
#endif
{
    Widget shell;
    Pixmap iconPmap;
    XGCValues xgc;
    XtAppContext appContext;
    XrmValue value;
    XrmDatabase db = NULL;
    char buf[256];
    XVisualInfo vinfo;
    XVisualInfo *vPtr;

/*** INIT GRASS LIBRARY, XT TOOLKIT AND APPLCATION ***/

    G_gisinit(argv[0]);     /* GRASS Library */
    XtToolkitInitialize();  /* Xt Toolkit */

    Global.appContext = XtCreateApplicationContext();
    Global.display = XtOpenDisplay(Global.appContext,NULL,NULL,"XGrass", 
       initTable,XtNumber(initTable),&argc,argv);
    if (Global.display == NULL) 
       {
       fprintf(stderr, "Error: Can't open display.\n");
       exit(1);
       }
    Global.screenNo = DefaultScreen(Global.display);
    Global.visual = NULL;
    Global.visualClass = 99;

    /* Parse the rest of the command line */
    XrmParseCommand(&db, optTable, XtNumber(optTable), argv[0], &argc, argv);

/*** GET THE VISUAL AND VISUAL CLASS ***/

    /* Get the visual */
    if ( XrmGetResource(db, "xgre.visual", "XGrass.visual", buf,
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

/*** CREATE TOPLEVEL SHELL ***/

    Global.applShell = shell = XtVaAppCreateShell(argv[0],"XGrass", 
        applicationShellWidgetClass, Global.display, 
        XtNscreen,DefaultScreenOfDisplay(Global.display),NULL);

    /* get the colormap  */
    Global.cmap = DefaultColormap(Global.display, Global.screenNo);

    /* get the screen pointer */
    Global.screenPtr = XtScreen(shell);
#   ifdef DEBUG
    printf("Got the screen ptr\n");
#   endif

    CreateLayout(shell);
#   ifdef DEBUG
    printf("back from CreateLayout\n");
#   endif

    XtRealizeWidget(shell);

/*** INIT XGRASS DISPLAY LIBRARY ***/

#   ifdef DEBUG
    printf("About to init the Xgd library\n");
#   endif

    XgdInit(Global.display, Global.cmap, vPtr, True);
    Global.imageGC = XCreateGC(Global.display,
       XtWindow(Global.imageArea),0,&xgc);
    XGetGCValues(Global.display,Global.imageGC,
       GCFillStyle|GCLineStyle|GCForeground|GCBackground|GCLineWidth,&xgc);
    Global.obj = XgdCreateObject(XGD_GEOFRAME);

#   ifdef DEBUG
    printf("About to init our friend, Mr. Object\n");
#   endif

    XgdInitObject(Global.display,XtWindow(Global.applShell),Global.obj,
       xgc.fill_style,xgc.line_style, xgc.foreground,
       xgc.background,xgc.line_width);

#   ifdef DEBUG
    printf("About to allocate colors\n");
#   endif

    /* allocate space for a color lookup table */
    if (!(Global.obj->Obj.GeoFrame.lookup_tbl =
       (Pixel*)calloc(300,sizeof(u_long))))
       XgdError("can't allocate color table");
    if (!(Global.obj->Obj.GeoFrame.borrowed = 
       (Pixel *)calloc(300,sizeof(Pixel))))
       XgdError("can't allocate color table");
    
    /* reserve highlight color */
    Global.highlight = XgdGetHighlightColor();

    /* reserve vector colors */
    Global.vectColors = 
        XgdInitVectorColors(Global.display,&Global.numVectColors);
    if ( Global.numVectColors == 0 ) 
       {
       fprintf(stderr, "Error: Couldn't allocate vector colors.\n");
       }

/*** INIT GLOBALS ***/

    /* init default brush */
    Global.brushCat = 0;
    Global.brushRows = 2;
    Global.brushCols = 2;
    Global.brushHotRow = 0;
    Global.brushHotCol = 0;
    BuildDefaultBrush();

    /* init dialog and window flags */
    Global.FbrushCatD    = False;
    Global.FbrushCatD    = False;
    Global.FbrushColorD  = False;
    Global.FbrushEditD   = False;
    Global.FbrushCreateD = False;
    Global.FbrushLoadD   = False;
    Global.FbrushSaveD   = False;
    Global.FeditRangeD   = False;
    Global.FeditCatsD    = False;
    Global.FfileOpenD    = False;
    Global.FfileSaveD    = False;
    Global.FfileResumeD  = False;
    Global.FviewZoomW    = False;
    Global.FviewIndexW   = False;
    Global.zoomLoaded    = False;
    Global.segtype       = XGRE_RASTER_REG;
    Global.mode          = XGRE_UNLOADED;

    SetEditMode(XGRE_UNLOADED);

    /* create program icon */
    iconPmap = XCreateBitmapFromData(Global.display,
       XtWindow(Global.applShell),(char*)icon_bits,icon_width,icon_height);
    XtVaSetValues(Global.applShell,XmNiconPixmap,iconPmap,NULL);

    /*** MEK WE DWEET! YEAH MON! ***/

    XtAppMainLoop(Global.appContext);
    return 0;
}
