/***********************************************************************

File     	:	CellEdit.c
Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	12 January 1990
Last Revised	:
Abstract 	:      	This file contains all the internal and external
			functions of the CellEditor widget

***********************************************************************/
#include <X11/copyright.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/file.h>
#include <sys/types.h>
#include <dirent.h>
#include <alloca.h>
#include <X11/Xatom.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/XawInit.h>
#include <X11/Xmu/Initer.h>
#include "CellEditP.h"
#include "PropSheetP.h"

static XtResource resources[] = {
#define offset(field) XtOffset(CellEditWidget, field)
    { XtNcellfile, XtCCellfile, XtRString, sizeof(String),
      offset(celledit.cellFileName), XtRString, (String)NULL },
    { XtNmapset, XtCMapset, XtRString, sizeof(String),
      offset(celledit.mapset), XtRString, (String)NULL },
    { XtNlocation, XtCLocation, XtRString, sizeof(String),
      offset(celledit.location), XtRString, (String)NULL },
    { XtNresize, XtCResize, XtRBoolean, sizeof(Boolean),
      offset(celledit.resize), XtRImmediate, (XtPointer)TRUE },
    { XtNcallback, XtCCallback, XtRCallback, sizeof(XtPointer),
      offset(celledit.callbacks), XtRCallback, (XtPointer)NULL },
    { XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
      offset(core.width), XtRDimension, (XtPointer)0 },
    { XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
      offset(core.height), XtRDimension, (XtPointer)0 },
#undef offset
    };

/* private data */

/* private functions */
static void 	Initialize(); 		/* initialize widget */
static void 	ClassInitialize();	/* initialize widget set */
static void 	Redisplay();		/* handle expose events */
static void 	Destroy();		/* destroy our widget */
static Boolean  SetValues();		/* false */
static void 	SetColors();		/* load grass color info */
static void 	LoadGrassInfo();	/* load grass info */
static void 	LoadPixmap();		/* load image */
static int	Min();			/* used by qsort */
static void 	ParseRow();		/* parse a row of data */
static void 	SetSize();		/* reset our size */
static unsigned short GetDataIndex();	/* get color index for data */

/* prop sheet functions NOTE: see PropSheetP.h */
static unsigned short CalcLabelColor();	/* figure label text color */
static void	BuildPropSheet();	/* build the whole property sheet */

static void	BuildPairsPart();	/* build data pairs part of PS */
static void	StretchButtons();	/* stretch data pairs to equal width */
static void	ShowPropSheet();	/* unwritten */
static void	HidePropSheet();	/* unwritten */
static void	SetPairProc();		/* data pairs btn callback */
static void	PairsPageProc();	/* pairs page up/dwn btn callback */

static void	BuildColrPart();	/* build color part of PS */
static void	ColrPageProc();		/* color page up/dwn btn callback */
static void 	SetColrProc();		/* color callback */

static void	BuildStatePart();	/* build current state part */
static void 	SetCurrData();		/* update current value */
static void 	SetCurrColr();		/* update current color */
static void 	UpdateState();		/* update entire state */

static void	BuildBrushPart();	/* build brush part */
static void	BuildBox();		/* build a brush box */
static void	LoadBrush();		/* load a new brush */

/* stuff we want to keep private, that aren't functions */
static int loaded = FALSE;
static int imageW;
static int imageH;

/* action calls */
static void 	Edit();			/* edit */
static void 	Refresh();		/* refresh the screen */

static XtActionsRec actionList[] =
    {
	{"Edit",		Edit },
	{"Refresh",		Refresh },
	{NULL,			NULL }	/* closure */
    };

static char translations[] =
"Shift<Btn1Up>:		Edit() \n\
 Shift<Btn2Up>:		Refresh()";


CellEditClassRec cellEditClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &widgetClassRec,
    /* class_name		*/	"CellEdit",
    /* widget_size		*/	sizeof(CellEditRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	actionList,
    /* num_actions		*/	XtNumber(actionList),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	XtInheritResize,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	translations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* simple fields */
    /* change_sensitive         */      XtInheritChangeSensitive
  },

  { /* cellEdit fields */
    /* empty			*/	0
  },
};

WidgetClass cellEditWidgetClass = (WidgetClass)&cellEditClassRec;

/*********************** Private Functions **********************/

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void ClassInitialize()

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	14 January 1990
Last Revised	:
Abstract 	:	This function initializes the vendor portion
			of the widget.
Returns  	:	None.

***********************************************************************/
static void ClassInitialize()
    {
    XawInitializeWidgetSet();
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void Initialize(request, new, args, num_args)
Args	 	:	    Widget request; --  UNUSED
			    Widget new; -- The new widget
  	    		    ArgList args; -- UNUSED 
			    Cardinal *num_args; -- UNUSED 

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	12 January 1990
Last Revised	:
Abstract 	:	Take care of everything that needs to be
			intialized. 

Returns  	:	None.

***********************************************************************/
static void Initialize (request, new, args, num_args)
    Widget request;
    Widget new;
    ArgList args;	/* UNUSED */
    Cardinal *num_args;	/* UNUSED */
    {
    CellEditWidget cew = (CellEditWidget)new;
    Display *dpy = XtDisplay(cew);
    int screenNum = DefaultScreen(dpy);
    int depth = DefaultDepth(dpy, screenNum);

    /* This gets us a temporary file in 
     * the grass hierachy where there 
     * is likely to be more room 
     */
    cew->celledit.segFileName = G_tempfile();


    /* Fill these fields of our widget */
    cew->celledit.mapset = G_store(G_mapset());
    cew->celledit.location = G_store(G_location());
    cew->celledit.cellFilePath = G_store(MakeCellPath());
    /* no files yet */
    cew->celledit.cellFileName = (char *)NULL;
    cew->celledit.saveFileName = (char *)NULL;

    /* Make our cmap point to the default */
    cew->core.colormap = DefaultColormap(dpy, screenNum);

    /* Initialize CellEdit counters  */
    cew->celledit.numPixels = 0;
    cew->celledit.numData = 0;

    cew->celledit.propShell = (Widget)NULL;

    /* Initialize Property sheet flags */

    /* Initialize the zoom factor */
    cew->celledit.cell_res = 1;

    /*
     * This widget uses a pixmap to always hold the contents of it's
     * window. While expensive, it performs quite well for redraws.
     */
    cew->celledit.image_backup = 
   	 XCreatePixmap(dpy, RootWindow(dpy, screenNum), cew->core.width, 
		       cew->core.height, depth);

    /* Create and associate our GC */
    cew->celledit.gc = XCreateGC(dpy, RootWindow(dpy, screenNum), 0, NULL);

    /* initialize pixmap */
    XSetForeground(dpy, cew->celledit.gc, cew->core.background_pixel);

    XFillRectangle(dpy, cew->celledit.image_backup, cew->celledit.gc, 0, 0,
		   cew->core.width, cew->core.height);
    }


/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void Redisplay(w, event, region)
Args	 	:	    Widget w; -- Our widget
  	    		    XEvent event; -- expose event
			    Region region; -- exposed region

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	23 February 1990
Last Revised	:
Abstract 	:	Handles all expose events on our widget
Returns  	:	None.

***********************************************************************/
static void Redisplay(w, event, region) 
    Widget w;
    XEvent *event;	/* UNUSED */
    Region region;
    {
    CellEditWidget cew = (CellEditWidget)w;
    Display *dpy = XtDisplay(cew);
    XRectangle rect;

    /* translate region to rect for redraw */ 
    XClipBox(region, &rect);

    /* Copy the exposed rectangle 
     * from our image_backup to 
     * the window. For a very speedy redraw :-)
     */
    XCopyArea(dpy, cew->celledit.image_backup, XtWindow(cew),
	     cew->celledit.gc,rect.x, rect.y, rect.width, rect.height, 
	     rect.x, rect.y);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static Boolean SetValues()

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	23 February 1990
Last Revised	:
Abstract 	:	I am not allowing this widget to be reconfigured
			after creation. Yet...
Returns  	:	False.

***********************************************************************/
static Boolean SetValues()
    {
    return (FALSE);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void Destroy(w, call, client)
Args	 	:	    Widget w1; -- Our widget
  	    		    XtPointer call; -- call data
  	    		    XtPointer client; -- client data

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	23 February 1990
Last Revised	:
Abstract 	:	Free all our resources and remove our temp file
Returns  	:	None.

***********************************************************************/
static void Destroy(w, call, client)
    Widget w;
    XtPointer call, client; /* UNUSED */
    {
    CellEditWidget cew = (CellEditWidget)w;
    Display *dpy = XtDisplay(cew);
    char rmStr[80];

    if (loaded)	/* if we've loaded a file */
	{
	/* free these arrays */
	printf("trying to destroy celleditor widget\n");
	free(cew->celledit.pixelList);
	free(cew->celledit.dataList);
	free(cew->celledit.colors);
	/* free the gc */
	XFreeGC(dpy, cew->celledit.gc);
	/* release the segment */
	segment_release(&(cew->celledit.segment));
	/* close the segment file */
	close(cew->celledit.segFileDesc);
	/* remove the segment file */
	if(!unlink(cew->celledit.segFileName))
	    fprintf(stderr, "tmp files removed\n");
	else
	    fprintf(stderr, "Unable to remove tmp file\n");
	/*****
	sprintf(rmStr, "/bin/rm -f %s\n", cew->celledit.segFileName);
	system(rmStr);
	******/
	/* free this hunk of server space  */
	XFreePixmap(dpy, backPix); 
	}
    XFreePixmap(dpy, cew->celledit.image_backup);
    XFlush(dpy);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	SetColors(w)
Args	 	:	    Widget w; -- Our Widget

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	23 February 1990
Last Revised	:
Abstract 	:	Fill the color table, and the pixel array.

			Currently, this routine trys to use the default
			X color map. This should be rewritten to use 
			either the default, or a virtual colormap based on
			runtime resource specifications. This
			change will probably happen somewhere else.
Returns  	:	None.

***********************************************************************/
static void SetColors(w)
    Widget w;
    {
    CellEditWidget cew = (CellEditWidget)w;
    Display *dpy = XtDisplay(cew);
    XColor xcolor;
    Boolean wrap;				/* pixel wrap flag */
    register int i, j;
    static int pixelCount;

    /* Check if we've been here before?
     * if so, free the old colors.
     */

    if( cew->celledit.numPixels != 0)
	{
	for (i = 0; i < pixelCount; i++)
	    {
	    XFreeColors(dpy, cew->core.colormap, &(cew->celledit.pixelList[i]), 
		    1, 0);
	    }
        pixelCount = 0;
	free(cew->celledit.pixelList);
	free(cew->celledit.dataList);
	}

    /* initializes the color structure */
    G_init_colors(&(cew->celledit.colors));
    /* allocate memory and read in grass colors */
    switch(G_read_colors(cew->celledit.cellFileName,
			  cew->celledit.mapset, &(cew->celledit.colors)))
	{
	case 0:
	    G_warning("Cell File has no color table, using default\n");
	    break;
	case -1:
	    G_fatal_error("Unable to read color file \n");
	    break;
	}

    /* set loop condition */
    cew->celledit.numPixels = (cew->celledit.colors.max + 2) - 
			       cew->celledit.colors.min;
    cew->celledit.min_color = cew->celledit.colors.min;

    /* Allocate space for these parts of the celleditor */
    cew->celledit.pixelList = 
	(unsigned long *)G_calloc(cew->celledit.numPixels, sizeof(long));
    cew->celledit.dataList = 
	(CELL *)G_calloc(cew->celledit.numPixels, sizeof(CELL));
    cew->celledit.numData = 0;

    /* load r0,g0,b0 */
    xcolor.red = (unsigned short)((int)cew->celledit.colors.r0 * 257);
    xcolor.green = (unsigned short)((int)cew->celledit.colors.g0 * 257);
    xcolor.blue = (unsigned short)((int)cew->celledit.colors.b0 * 257);
    xcolor.flags = (DoRed | DoGreen | DoBlue);

    /* put it in the colormap */
    if (XAllocColor(dpy, cew->core.colormap, &xcolor) == 0)
	{
	/* If we can't do one, don't bother */
	G_fatal_error("Unable to allocate any colors \n");
	}
    else
	{
	cew->celledit.pixelList[0] = xcolor.pixel;
	pixelCount = 1;
	}

    /* Loop to load the rest */ 
    for (i = 1; i < cew->celledit.numPixels; i++ )
        {
	/* these colors values were returned by G_read_colors(); */
	xcolor.red = (unsigned short)(
		cew->celledit.colors.red[i - cew->celledit.colors.min] * 257);
	xcolor.green = (unsigned short)((int)
		cew->celledit.colors.grn[i - cew->celledit.colors.min] * 257);
	xcolor.blue = (unsigned short)((int)
		cew->celledit.colors.blu[i - cew->celledit.colors.min] * 257);

	/* This should work with reasonably sized color tables, but I
	 * might have to change this for a 50,000 color colortable, 
	 * (ie, I can't count of memory being available for a 50,000 entry
	 * array 
	 */

	/* 
	 * Try each one, because we need to 
	 * know when we can't allocate more 
	 */
	if (XAllocColor(dpy, cew->core.colormap, &xcolor) == 0)
	    {
	    /* We can't allocate  more, set the flag */
	    wrap = True;
	    break;
	    }
	else
	    {
	    /* store away the returned pixel indecies */
	    cew->celledit.pixelList[pixelCount] = xcolor.pixel;
	    pixelCount++;
	    }

        }
    /* 
     * If wrap == True, then we ran out of pixels
     * in the default colormap, and we need to "wrap" them so that
     * there is a color for each data item, even it that color
     * is also used for another data value as well.
     * for example: if there are 30 data values, and we can
     * only allocate 25 colors, then color 26 will equal
     * color 1, and color 27, will equal color 2. I've
     * set color 0 to r0, g0, b0. Since it seemed appropriate
     * to Grass to make a destinction, I don't map it to any 
     * data value. We felt it was important to use the default 
     * colormap so as not to hose the other clients that are running
     */

    if (wrap == True)
	{
	/* i is where we left off, now it helps us count */
	for (j = i, i = 1; j < cew->celledit.numPixels; j++, i++ )
	    {
	    cew->celledit.pixelList[j] = 
		cew->celledit.pixelList[j - (j-i)];
	    }
	}
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	staic int LoadGrassInfo(w)
Args	 	:	    Widget w; -- the celleditor widget

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	22 January 1990
Last Revised	:
Abstract 	:	Initializes grass stuff, get all the grass info
			we need into the widget's structure.
Returns  	:	None.

***********************************************************************/
static void LoadGrassInfo(w)
    Widget w;
    {
    CellEditWidget cew = (CellEditWidget)w;
    struct Cell_head cellHd;
    struct Cell_head window;
    static Boolean init = False;
    int cols, rows;
    int deltaX, deltaY;
    int deltaCells;
    int srows, scols;

    if (init)	/* if reload */
	{
	char rmStr[80];
	segment_release(&(cew->celledit.segment));
	close(cew->celledit.segFileDesc);
	/* remove the cell file */
	if(unlink(cew->celledit.segFileName))
	    G_fatal_error("Unable to remove old segment file file\n");
	init = True;
	}
    /* 
     * We need to build a FULL size map.
     * So, first get the cell header info
     */
    G_get_cellhd(cew->celledit.cellFileName, cew->celledit.mapset, &cellHd);
    G_get_window(&window);

    /* save this away, we'll restore it when we're done */
    cew->celledit.user_window = window; 

    /* Make sure user's window has full resolution */ 
    if (( window.ew_res != cellHd.ew_res) ||
       ( window.ns_res != cellHd.ns_res) )
	{
	window.ew_res = cellHd.ew_res; 
	window.ns_res = cellHd.ns_res;
	/* Put it into the active window */
	G_set_window(&window);
	}

    /* now we're sure the resolution is correct, so
     * get rows and cols 
     */
    cew->celledit.cols = cols = G_window_cols();
    cew->celledit.rows = rows = G_window_rows();

    /* here we do some quick calculations */
    deltaX = (int)floor((double)(MAX_WIDTH/cols));
    deltaY = (int)floor((double)(MAX_HEIGHT/rows));
    deltaCells = (int)MIN(deltaX, deltaY);

    /* we will use this to "fill" the window */
    if (deltaCells > 1)
	cew->celledit.cell_res = deltaCells;
    else
       cew->celledit.cell_res = 1;

    /* Michael Shapiro suggests these sizes for optimal performance
     * I defer to his expertise SEG_SIZE = 64.
     */
    srows = SEG_SIZE;
    scols = SEG_SIZE;

    /* create/open segment file */
    cew->celledit.segFileDesc = open(cew->celledit.segFileName, O_RDWR|O_CREAT,
				    0666);
    if (cew->celledit.segFileDesc < 0)
	{
	G_fatal_error("Unable to create Segment File!\n");
	}
    if (segment_format(cew->celledit.segFileDesc, cew->celledit.rows,
		   cew->celledit.cols,
		   srows,
		   scols,
		   sizeof(CELL)) < 1)
	{
	G_fatal_error("Unable to format Segment File!\n");
	}

    /* close the segment for now */
    close(cew->celledit.segFileDesc);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void LoadPixmap(w)
Args	 	:	    Widget w -- the celleditor widget

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	23 January 1990
Last Revised	:
Abstract 	:	Loads an image into the pixmap.
Returns  	:	None.

***********************************************************************/
static void LoadPixmap(w)
    Widget w;
    {
    CellEditWidget cew = (CellEditWidget)w;
    CELL *cell_buf;
    Display *dpy = XtDisplay(cew);
    int screenNum = DefaultScreen(dpy);
    int depth = DefaultDepth(dpy, screenNum);
    int status = 0;
    int cellFileDesc;
    register int rowNum;


    /***** I don't think I need this...check it out
    XFreePixmap(dpy, cew->celledit.image_backup);

    cew->celledit.image_backup =
	     XCreatePixmap(dpy, RootWindow(dpy, screenNum), 
			   cew->core.width, cew->core.height,
			   depth);
    *****/

    /* initialize it to our background resource color */
    XSetForeground(dpy, cew->celledit.gc, cew->core.background_pixel);
    XFillRectangle(dpy, cew->celledit.image_backup, cew->celledit.gc,
		   0, 0, cew->core.width, cew->core.height);

    /* Now, open the cell file */
    cellFileDesc = G_open_cell_old(cew->celledit.cellFileName, 
			    cew->celledit.mapset);

    /* allocate space for one line */
    cell_buf = G_allocate_cell_buf();

    cew->celledit.segFileDesc = open(cew->celledit.segFileName, O_RDWR);
    if (cew->celledit.segFileDesc < 0 )
	{
	G_fatal_error("Unable to open segement file, punting...\n");
	}

    status = segment_init(&(cew->celledit.segment), cew->celledit.segFileDesc, 
		          NUM_SEGS);
    if (status < 0 )
	G_fatal_error("Unable to intializes  segement file, punting...\n");

    /* row by row,
     * a) get map row,
     * b) put the row into the segment file 
     * c) draw the row to the screen
     */
    for ( rowNum = 0; rowNum < cew->celledit.rows; rowNum++)
	{
	G_get_map_row_nomask(cellFileDesc, cell_buf, rowNum);
	segment_put_row( &(cew->celledit.segment) , cell_buf, rowNum);
	ParseRow(cew, cell_buf, rowNum);
	}

    /* flush the segment puts */
    segment_flush(&(cew->celledit.segment));

    /* sort data list which is created in ParseRow */
    qsort(cew->celledit.dataList, cew->celledit.numData, sizeof(CELL), Min);
    free(cell_buf);
    /* close the cell file for now */
    G_close_cell(cellFileDesc);
    }

/* sorting routine for qsort */
static int Min(i1, i2)
    CELL *i1;
    CELL *i2;
    {
    int j1 = *i1;
    int j2 = *i2;
    if ( MIN(j1, j2) == j1)
	return (-1);
    else 
	return (1);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void ParseRow(w, row, rowNum)
Args	 	:	    Widget w; -- Our Widget
  	    		    CELL *row; -- one data row
			    int rowNum; -- which row

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:	23 February 1990
Abstract 	:	Draw a single row as effeciently as possible
			We drawing all continuous data values at once
Returns  	:	None.

***********************************************************************/
static void ParseRow(w, row, rowNum)
    Widget w;
    CELL *row;
    int rowNum;
    {
    CellEditWidget cew = (CellEditWidget)w;
    Display *dpy = XtDisplay(cew);
    register int i = 0;
    register int col1 = 0;
    register int col2 = 1;
    int pixel = 0;


    /* make sure line style is correct */
    XSetLineAttributes(dpy, cew->celledit.gc, (unsigned int)1, LineSolid,
			CapButt, JoinMiter);

    /* we're operating on a single row of data */
    /* loop through the columns until one from the end */
    while(i < cew->celledit.cols)
	{
	/* if they're equal, next column */
	while (row[i] == row[i+1])
	    {
	    i++;
	    col2++;
	    }
	/* get the right color for this data value */
	pixel = GetDataIndex(cew, row[col1]);
	/* ready, set, draw! */
	XSetForeground(dpy, cew->celledit.gc, pixel);
	if (cew->celledit.cell_res == 1) 
	    { /* a resolution of 1 draws with lines */
	    XDrawLine(dpy, cew->celledit.image_backup, cew->celledit.gc,
		      col1, rowNum, col2, rowNum);
	    XDrawLine(dpy, XtWindow(cew), cew->celledit.gc,
		      col1, rowNum, col2, rowNum);
	    }
	else
	    { /* a resolution greater than 1 draws with rectangles */
	     XFillRectangle(dpy, cew->celledit.image_backup,
			   cew->celledit.gc,
			   col1*cew->celledit.cell_res,
			   rowNum*cew->celledit.cell_res,
			   (col2 - col1)*cew->celledit.cell_res,
			   cew->celledit.cell_res);
	     XFillRectangle(dpy, XtWindow(cew),
			   cew->celledit.gc,
			   col1*cew->celledit.cell_res,
			   rowNum*cew->celledit.cell_res,
			   (col2 - col1)*cew->celledit.cell_res,
			   cew->celledit.cell_res);
	    }
	col1 = col2++;
	i++;
	}
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void SetSize(w)
Args	 	:	    Widget w; -- Our widget

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:	23 February 1990
Abstract 	:	Try to resize to the our optimal size.
Returns  	:	None.

***********************************************************************/
static void SetSize(w)
    {
    CellEditWidget cew = (CellEditWidget)w;
    XtGeometryResult result;
    Dimension return_width, return_height;
    Dimension request_width = (cew->celledit.cols * cew->celledit.cell_res);
    Dimension request_height = (cew->celledit.rows * cew->celledit.cell_res);

    /* We want our widget to be the exact size of it's image,
     * so first we ask
     */
    /* first ask our parent for a resize */
    result = XtMakeResizeRequest(cew, request_width, request_height,
		 &return_width, &return_height);
    /* then check the answer */
    switch(result)
	{
	case XtGeometryAlmost:
	    {
	    /* if almost is good enough, 
	     * accept by making an updated request
	     */
	    if (( return_width >= request_width) && 
		(return_height >= request_height))
		XtMakeResizeRequest(cew, return_width, return_height,
				    NULL, NULL);
	    break;
	    }
	/* Yes means it's been taken care of, therefore do nothing */
	case XtGeometryYes:
	    break;
	/* No means we've been denied, therefore do nothing */
	case XtGeometryNo:
	    break;
	}
    XFlush(XtDisplay(cew));
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void BuildPropertySheet(w)
Args	 	:	    Widget w; -- Our widget

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	25 February 1990
Last Revised	:
Abstract 	:	Build the entire property sheet with callbacks,
			and everything. Most of these widgets are defined
			statically in PropSheetP.h More files are included
			there as well. It is my hope that that file will 
			give a little organization to this beast.
Returns  	:	None.

***********************************************************************/
static void BuildPropSheet(w)
    Widget w;
    {
    CellEditWidget cew = (CellEditWidget)w;

    /* create popup shell (the top level of the porpert sheet (PS) */
    cew->celledit.propShell = 
	XtVaCreatePopupShell("Property Sheet", topLevelShellWidgetClass, cew,
			    XtNallowShellResize, (XtArgVal)TRUE, NULL);

    /* top widget (with a window) of the property sheet */
    propForm = 
	XtVaCreateManagedWidget("propForm", formWidgetClass,
			      cew->celledit.propShell, NULL);

 
    /* create the preset pairs part of the property sheet */
    BuildPairsPart(cew);	

    /* create the colors part of the property sheet */
    BuildColrPart(cew);
    /* create the  current state part of the property sheet */
    BuildStatePart(cew);
    /* create the current brush part of the property sheet */
    BuildBrushPart(cew);

    /* realize and map the PS */
    XtRealizeWidget(cew->celledit.propShell);
    XtMapWidget(cew->celledit.propShell);

    /* Create an InputOnly window to shield our time consuming
     *  routines from impatient users like Chuck. 
     */

    /* ./lib/Xutils/busy.c */
    busy = (Window)CreateBusyWindow(propForm);
    }

/***********************************************************************

File     	:	CellEdit.c.c
Function 	:	static void BuildPairsPart(w)
Args	 	:	    Widget w; -- Our widget

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	15 March 1990
Last Revised	:
Abstract 	:	Build the preset data pairs section of the PS.
Returns  	:	None.

***********************************************************************/
static void BuildPairsPart(w)
    Widget w;
    {
    CellEditWidget cew = (CellEditWidget)w;
    Display *dpy = XtDisplay(cew);
    int screen = DefaultScreen(dpy);
    register int i;
    unsigned long pixel;
    unsigned long labelPixel;
    int status;
    Boolean haveData = TRUE;
    char *pairsString;
    char label[80];
    int longest = strlen(pairsTitleStr);
    Widget long_widget;
    Arg arg[1];
    int numLabels = DATA_WIDGETS;
    int numPages;


    /* calculate the number of pages of data values */
    numPages = (int)ceil((double)cew->celledit.numData / (double)DATA_WIDGETS);

    /* pairs section top form */
    pairsForm = 
	XtVaCreateManagedWidget("pairsForm", formWidgetClass, propForm,
				XtNresizable, (XtArgVal)TRUE, NULL);

    /* Since all the buttons will have color, or certainly could,
     * I'm going to use a background pixmap on my "control buttons",
     * and labels to make them distinct. It's declared in PropSheetP.h
     */
    backPix = 
	XCreatePixmapFromBitmapData(dpy, RootWindow(dpy, screen), gray3_bits, 
				    (unsigned int)gray3_width, 
				    (unsigned int)gray3_height,
				    BlackPixel(dpy, screen),
				    WhitePixel(dpy, screen),
				    DefaultDepth(dpy, screen));
    if (backPix == None)
	G_fatal_error("No pixmap memory\n");

    /* this label is the title for this section of the sheet */
    XtSetArg(pairArgs[1], XtNborderWidth, (XtArgVal)1);
    XtSetArg(pairArgs[2], XtNforeground, (XtArgVal)BlackPixel(dpy, screen));
    XtSetArg(pairArgs[3], XtNbackground, (XtArgVal)WhitePixel(dpy, screen));
    XtSetArg(pairArgs[4], XtNbackgroundPixmap, (XtArgVal)backPix);
    pairsTitle = 
	XtCreateManagedWidget("dataTitle", labelWidgetClass, pairsForm,
			      pairArgs, XtNumber(pairArgs));

    /* track the longest widget */
    long_widget = pairsTitle;

    /* create the buttons, and data strings */
    status =  G_read_cats(cew->celledit.cellFileName, cew->celledit.mapset, 
			  &(cew->celledit.cats));

    /* make sure there is a cats file */
    if (status < 0 )
	G_warning("No categories file, doing my best...\n");

    /* Figure out how many labels will be needed for this page */
    if (cew->celledit.numData < DATA_WIDGETS)
	numLabels = (cew->celledit.numData % DATA_WIDGETS);
    if (numLabels == 0)
	G_fatal_error("There is no data\n");

    XtSetArg(pairArgs[4], XtNbackgroundPixmap, 
			      (XtArgVal)XtUnspecifiedPixmap);
    for( i = 0; i < DATA_WIDGETS; i++ )
	{
	if (i < numLabels)
	    {
	    /* first get the color */
	    pixel = GetDataIndex(cew, cew->celledit.dataList[i]);
	    /* make first part of the label string */
	    sprintf(label,"%d -- ",cew->celledit.dataList[i]);
	    /* free it every time */
	    free(pairsString); 		/* make sure this is fresh */
	    /* build our label */
	    /* data -- name */
	    pairsString = 
		(char *)G_store((char *)G_get_cat(cew->celledit.dataList[i],
						  &(cew->celledit.cats)));
	    if ( streq(pairsString,"") )
		{
		strcat(label, NullNameString);
		}
	    else
		{
		strcat(label, pairsString);
		}
	    /* for each data value, we want to create a command
	     * button with it's label set to a foreground color
	     * we can see, and it's backgound set to the proper,
	     * data color 
	     */

	    labelPixel = CalcLabelColor(cew, pixel);

	    /* Now, create a button */
	    XtSetArg(pairArgs[0], XtNlabel, (XtArgVal)label);
	    XtSetArg(pairArgs[2], XtNforeground, (XtArgVal)labelPixel);
	    XtSetArg(pairArgs[3], XtNbackground, (XtArgVal)pixel);
	    XtSetArg(pairArgs[9], XtNsensitive, (XtArgVal)TRUE);
	    }
	else	/* end of a page, fill with blank (insensitive) entries */
	    {
	    XtSetArg(pairArgs[0], XtNlabel, (XtArgVal)NullDataString);
	    XtSetArg(pairArgs[2], XtNforeground,
				    (XtArgVal)BlackPixel(dpy, screen));
	    XtSetArg(pairArgs[3], XtNbackground,
				    (XtArgVal)WhitePixel(dpy, screen));
	    /* turn 'm off */
	    XtSetArg(pairArgs[9], XtNsensitive, (XtArgVal)FALSE);
	    }
	/* make sure they stack in order */
	if ( i == 0)
	    XtSetArg(pairArgs[5], XtNfromVert, (XtArgVal)pairsTitle);
	else
	    XtSetArg(pairArgs[5], XtNfromVert, (XtArgVal)pairs[i-1]);
	pairs[i] = 
	    XtCreateManagedWidget("pairsButton", commandWidgetClass,
				  pairsForm, pairArgs, XtNumber(pairArgs));

	/* cew holds data we need, so set it to client data */
	/* add a cllaback, so we know someone hit this button */
	XtAddCallback(pairs[i], XtNcallback, SetPairProc, (XtPointer)cew);

	/* We want all the buttons to be the same width, so I'm
	 * finding the longest in the creation loop, and we'll stretch
	 * the rest later
	 */
	if (strlen(label) > longest)
	    {
	    longest = strlen(label);
	    long_widget = pairs[i];
	    }
	}
    /* pairsPgNum tells us where in the data list we are, so
     * we scroll in the right direction 
     */
    pairsPgNum = 1;

    /* now, page_up */
    /* these too get a background tile */
    XtSetArg(pairArgs[0], XtNlabel, (XtArgVal)pageUpStr);
    XtSetArg(pairArgs[2], XtNforeground, (XtArgVal)BlackPixel(dpy, screen));
    XtSetArg(pairArgs[3], XtNbackground, (XtArgVal)WhitePixel(dpy, screen));
    XtSetArg(pairArgs[4], XtNbackgroundPixmap, (XtArgVal)backPix);
    XtSetArg(pairArgs[5], XtNfromVert, (XtArgVal)pairs[9]);
    pairsPgUp = 
	XtCreateManagedWidget("pageUp", commandWidgetClass, pairsForm,
			      pairArgs, XtNumber(pairArgs));
    XtAddCallback(pairsPgUp, XtNcallback, PairsPageProc, (XtPointer)cew);
    if (numPages == 1)
	{ /* disable paging */
	XtSetArg(arg[0], XtNsensitive, (XtArgVal)FALSE);
	XtSetValues(pairsPgUp, arg, (Cardinal)1);
	}

    /* now page down */
    XtSetArg(pairArgs[0], XtNlabel, (XtArgVal)pageDownStr);
    XtSetArg(pairArgs[5], XtNfromVert, (XtArgVal)pairsPgUp);
    pairsPgDown = 
	XtCreateManagedWidget("pageDown", commandWidgetClass, pairsForm,
			      pairArgs, XtNumber(pairArgs));
    /* again, pass cew */
    XtAddCallback(pairsPgDown, XtNcallback, PairsPageProc, (XtPointer)cew);
    if (numPages == 1)
	{ /* disable paging */
	XtSetArg(arg[0], XtNsensitive, (XtArgVal)FALSE);
	XtSetValues(pairsPgDown, arg, (Cardinal)1);
	}

    /* Now, stretch the data buttons to the right width */
    StretchButtons((Widget)long_widget);
    XFlush(dpy);
    /* realize the widgets */
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void BuildColorPart(w)
Args	 	:	    Widget w; -- Our widget 

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	15 March 1990
Last Revised	:
Abstract 	:	Build the color section of the PS
Returns  	:	None.

***********************************************************************/
static void BuildColrPart(w)
    Widget w;
    {
    CellEditWidget cew = (CellEditWidget)w;
    Display *dpy = XtDisplay(cew);
    int screen = DefaultScreen(dpy);
    unsigned int depth = DefaultDepth(dpy, screen);
    register int i;
    int numColrs = COLR_WIDGETS;
    unsigned short pixel;
    unsigned short labelPixel;
    char labelStr[80];
    Dimension width;
    Arg getArgs[1];
    Arg arg[1];
    int numPages;
    
    /* calc how many pages */
    numPages = (int)ceil((double)cew->celledit.numPixels/(double)COLR_WIDGETS);

    colrForm = XtVaCreateManagedWidget("colrForm", formWidgetClass, propForm, 
				XtNresizable,(XtArgVal)TRUE,
				XtNfromHoriz, (XtArgVal)pairsForm, NULL);

    /* Since all the buttons will have color
     * I'm going to use a background pixmap on my "control buttons",
     * and labels to make them distinct.
     */
    /* this label is the title for this section of the sheet */
    XtSetArg(colrArgs[0], XtNlabel, (XtArgVal)colrTitleStr);
    XtSetArg(colrArgs[1], XtNborderWidth, (XtArgVal)1);
    XtSetArg(colrArgs[2], XtNforeground, (XtArgVal)BlackPixel(dpy, screen));
    XtSetArg(colrArgs[3], XtNbackground, (XtArgVal)WhitePixel(dpy, screen));
    XtSetArg(colrArgs[4], XtNbackgroundPixmap, (XtArgVal)backPix);
    colrTitle =
        XtCreateManagedWidget("colrTitle", labelWidgetClass, colrForm,
                              colrArgs, 5 );

    /* colr title should be are widest widget,
     * so after we create it, query it's width
     */
    XtSetArg(getArgs[0], XtNwidth, (XtArgVal)&width);
    XtGetValues(colrTitle, getArgs, (Cardinal)1);
    colrBtnWidth = width;

    if ( cew->celledit.numPixels < DATA_WIDGETS)
	{
	numColrs = (int)(cew->celledit.numPixels % COLR_WIDGETS);
	}

    /* now the color buttons */
    XtSetArg(colrArgs[1], XtNborderWidth, (XtArgVal)1);
    XtSetArg(colrArgs[2], XtNforeground, (XtArgVal)XtDefaultForeground);
    XtSetArg(colrArgs[3], XtNbackground, (XtArgVal)XtDefaultBackground);
    XtSetArg(colrArgs[4], XtNbackgroundPixmap, (XtArgVal)XtUnspecifiedPixmap);
    XtSetArg(colrArgs[5], XtNwidth, (XtArgVal)width);
    XtSetArg(colrArgs[6], XtNheight, (XtArgVal)NULL);
    for(i = 0; i < COLR_WIDGETS; i++)
	{
	if (i == 0)
	    XtSetArg(colrArgs[8], XtNfromVert, (XtArgVal) colrTitle);
        else
	    XtSetArg(colrArgs[8], XtNfromVert, (XtArgVal) colrs[i -1]);
	if ( i < numColrs)
	    {
	    pixel = cew->celledit.pixelList[i];
	    labelPixel = CalcLabelColor(cew, pixel);

	    XtSetArg(colrArgs[2], XtNforeground, (XtArgVal)labelPixel);
	    XtSetArg(colrArgs[3], XtNbackground, (XtArgVal)pixel);
	    XtSetArg(colrArgs[7], XtNsensitive, (XtArgVal)TRUE);
	    sprintf(labelStr,"color: %d",i);
	    XtSetArg(colrArgs[0], XtNlabel, (XtArgVal)labelStr);
	    }
        else
	    XtSetArg(colrArgs[7], XtNsensitive, (XtArgVal)TRUE);
	colrs[i] = 
	    XtCreateManagedWidget("colrs", commandWidgetClass, colrForm, 
				  colrArgs, XtNumber(colrArgs));
	 /* cew holds data we need, so set it to client data */
	XtAddCallback(colrs[i], XtNcallback, SetColrProc, (XtPointer)cew);
	}

    /* now, page_up, and page_down */

    /* these too get a background tile */
    XtSetArg(colrArgs[0], XtNlabel, (XtArgVal)pageUpStr);
    XtSetArg(colrArgs[2], XtNforeground, (XtArgVal)BlackPixel(dpy, screen));
    XtSetArg(colrArgs[3], XtNbackground, (XtArgVal)WhitePixel(dpy, screen));
    XtSetArg(colrArgs[4], XtNbackgroundPixmap, (XtArgVal)backPix);
    XtSetArg(colrArgs[8], XtNfromVert, (XtArgVal)colrs[9]);
    colrPgUp =
        XtCreateManagedWidget("pageUp", commandWidgetClass, colrForm,
                              colrArgs, XtNumber(colrArgs));
    XtAddCallback(colrPgUp, XtNcallback, ColrPageProc, (XtPointer)cew);
    if (numPages == 1)
	{ /* disable paging */
	XtSetArg(arg[0], XtNsensitive, (XtArgVal)FALSE);
	XtSetValues(colrPgUp, arg, (Cardinal)1);
	}


    XtSetArg(colrArgs[0], XtNlabel, (XtArgVal)pageDownStr);
    XtSetArg(colrArgs[8], XtNfromVert, (XtArgVal)colrPgUp);
    colrPgDown =
        XtCreateManagedWidget("pageDown", commandWidgetClass, colrForm,
                              colrArgs, XtNumber(colrArgs));
    /* agan, pass cew */
    XtAddCallback(colrPgDown, XtNcallback, ColrPageProc, (XtPointer)cew);
    if (numPages == 1)
	{ /* disable paging */
	XtSetArg(arg[0], XtNsensitive, (XtArgVal)FALSE);
	XtSetValues(colrPgDown, arg, (Cardinal)1);
	}

    /* we are on the first page of colors */
    colrPgNum = 1;
    }
/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void StretchButtons(w)
Args	 	:	    Widget w; -- The longest button widget

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	15 March 1990
Last Revised	:
Abstract 	:	Stretch all the buttons out to the longest's
			width.
Returns  	:	None.

***********************************************************************/
static void StretchButtons(w)
    Widget w;		/* the longest widget */
    {
    register int i;
    Arg args[3];
    Dimension longest, width, b_width, borderWidth;
    int hDist, vDist;
    Dimension formWidth, formBW, formHeight;
    Dimension ret_width, ret_height;
    XtGeometryResult result;

    /* we have our longest widget, so get the info from it */
    XtSetArg(args[0], XtNwidth, &longest);
    XtSetArg(args[1], XtNborderWidth, &b_width);
    XtSetArg(args[2], XtNhorizDistance, &hDist);
    XtGetValues(w, args, (Cardinal)3);
    longest += 2 * b_width; 

    /* calc new form width */
    XtSetArg(args[0], XtNborderWidth, &formBW);
    XtGetValues(w, args, (Cardinal)1);
    formWidth = (longest + (2 * formBW) + (2 *hDist));

    /* Now, try to resize the form */
    XtSetArg(args[0], XtNheight, &formHeight);
    XtGetValues(XtParent(w), args, (Cardinal)1);

    /* request a resize */
    result = XtMakeResizeRequest(XtParent(w), formWidth, formHeight,
				 &ret_width, &ret_height);

    /* should always be yes, but just in case... */
    switch(result) 
	{
	case XtGeometryYes:
	    break;
	case XtGeometryNo:
	    break;
	case XtGeometryAlmost:
	    XtMakeResizeRequest(XtParent(w), ret_width, ret_height, NULL, NULL);
	    break;
	}

    /* Now reset all the data pairs widgets */
    XtSetArg(args[0], XtNborderWidth, &borderWidth);
    XtGetValues(pairsTitle, args, (Cardinal)1);
    width = longest -  2 * borderWidth;
    XtSetArg(args[0], XtNwidth, width);
    XtSetValues(pairsTitle, args, 1);
    for(i = 0; i < DATA_WIDGETS; i++)
	{
	XtSetArg(args[0], XtNborderWidth, &borderWidth);
	XtGetValues(pairs[i], args, 1);
	width = longest -  2 * borderWidth;
	XtSetArg(args[0], XtNwidth, width);
	XtSetValues(pairs[i], args, 1);
	}
    /* now page up and page down */
    XtSetArg(args[0], XtNborderWidth, &borderWidth);
    XtGetValues(pairsPgUp, args, 1);
    width = longest -  2 * borderWidth;
    XtSetArg(args[0], XtNwidth, width);
    XtSetValues(pairsPgUp, args, 1);

    XtSetArg(args[0], XtNborderWidth, &borderWidth);
    XtGetValues(pairsPgDown, args, 1);
    width = longest -  2 * borderWidth;
    XtSetArg(args[0], XtNwidth, width);
    XtSetValues(pairsPgDown, args, 1);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void SetPairProc(w, client, call)
Args	 	:	    Widget w; -- callback widget
  	    		    XtPointer client; -- Our widget
  	    		    XtPointer call; -- UNUSED

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	15 march 1990
Last Revised	:
Abstract 	:	Set the seleced preset data into the current 
			state.
Returns  	:	None.

***********************************************************************/
static void SetPairProc(w, client, call)
    Widget w;		/* the widget that called notify() */
    XtPointer client; 	/*  Our widget*/
    XtPointer call; 	/* UNUSED */
    {
    CellEditWidget cew = (CellEditWidget)client;
    register int i;
    int whichColor;

    /* which widget */
    for (i = 0; w != pairs[i]; i++);
    i += ((pairsPgNum -1) * DATA_WIDGETS);

    /* which color */
    whichColor = (cew->celledit.dataList[i] - cew->celledit.min_color) +1;

    /* set the data in current state part */
    SetCurrData(cew, i);
    /* set the color in the current state part */
    SetCurrColr(cew, whichColor);
    /* reconfigure state section */
    UpdateState();
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void SetColrProc(w, client, call)
Args	 	:	    Widget w; -- the called widget
  	    		    XtPointer call; -- Our widget
			    XtPointer client; -- UNUSED

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	15 March 1990
Last Revised	:
Abstract 	:	Color widget callback (update state section)
Returns  	:	None.

***********************************************************************/
static void SetColrProc(w, client, call)
    Widget w;		/* the widget that called notify() */
    XtPointer client; 	/*  Our widget*/
    XtPointer call; 	/* UNUSED */
    {
    CellEditWidget cew = (CellEditWidget)client;
    register int i;

    /* which widget */
    for (i = 0; w != colrs[i]; i++);
    i += ((colrPgNum -1) * COLR_WIDGETS);

    /* only update color part */
    SetCurrColr(cew, i);
    /* update state section */
    UpdateState();
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void ColorPageProc(w, client, call)
			    Widget w; -- the called widget
			    XtPointer call; -- Our widget
			    XtPointer client; -- UNUSED


Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:
Abstract 	:	Page Up/Dwn callback. Implements paging
Returns  	:	None.

***********************************************************************/
static void ColrPageProc(w, client, call)
    Widget w;		/* the widget that called notify() */
    XtPointer client; 	/* holds our editor widget */
    XtPointer call; 	/* UNUSED */
    {
    CellEditWidget cew = (CellEditWidget)client;
    register int shift;
    register int i;
    int numPages;
    int numColors = COLR_WIDGETS;
    int start, finish;
    unsigned short labelPixel;
    unsigned short pixel;
    char labelStr[80];
    Arg args[5];
    Display *dpy = XtDisplay(w);
    int screen = DefaultScreen(dpy);

    /* how many pages are there? */
    numPages = 
	(int)ceil((double)cew->celledit.numPixels / (double)COLR_WIDGETS);
    if ((numPages == 1) && (colrPgNum != 0))
	return;

    /* protect us from Chuck */
    (void)ShowBusy(dpy, busy); 

    /* don't do a re-layout until we're done with everything */
    XawFormDoLayout(XtParent(w), FALSE);
    XFlush(XtDisplay(w));	/* make sure this gets out */
    /* unlike the data pairs, we only care about the
     * color buttons, the controls should stay the same
     */
    if (w == colrPgDown)
        {
        colrPgNum++;
        if (( colrPgNum > numPages) || (colrPgNum == 0))
            { /* wrap */
            start = 0;
            finish = COLR_WIDGETS;
            if ( finish > cew->celledit.numPixels)
                numColors = (cew->celledit.numPixels % COLR_WIDGETS);
            colrPgNum = 1;
            }
        else
            {
            finish = colrPgNum * COLR_WIDGETS;
            start = finish - COLR_WIDGETS;
            if ( finish > cew->celledit.numPixels)
                numColors = (cew->celledit.numPixels % COLR_WIDGETS);
            }
        }
    else        /* w == colrPgUp */
        {
        colrPgNum--;
        if (colrPgNum == 0) /* go to last page */
            { /* back wrap */
            colrPgNum = numPages;
            finish = (colrPgNum) * COLR_WIDGETS;
            start = finish - COLR_WIDGETS;
            if ( finish > cew->celledit.numPixels)
                numColors = (cew->celledit.numPixels % COLR_WIDGETS);
            }
        else
            {
            finish = (colrPgNum) * COLR_WIDGETS;
            start = finish - COLR_WIDGETS;
            if ( finish > cew->celledit.numPixels)
                numColors = (cew->celledit.numPixels % COLR_WIDGETS);
            }
        }
    for(i = start, shift = 0; i < finish; i++, shift++)
        {
        if ( shift < numColors)
            {
	    pixel = cew->celledit.pixelList[i];
	    labelPixel = CalcLabelColor(cew, pixel);

            XtSetArg(args[0], XtNforeground, (XtArgVal)labelPixel);
            XtSetArg(args[1], XtNbackground, (XtArgVal)pixel);
            XtSetArg(args[2], XtNsensitive, (XtArgVal)TRUE);
            sprintf(labelStr,"color: %d", i);
            XtSetArg(args[3], XtNlabel, (XtArgVal)labelStr);
            XtSetArg(args[4], XtNwidth, (XtArgVal)colrBtnWidth);
            }
        else
	    {
            XtSetArg(args[0], XtNforeground, (XtArgVal)BlackPixel(dpy, screen));
            XtSetArg(args[1], XtNbackground, (XtArgVal)WhitePixel(dpy, screen));
            XtSetArg(args[2], XtNsensitive, (XtArgVal)FALSE);
	    G_strcpy(labelStr, "Undefined");
            XtSetArg(args[3], XtNlabel, (XtArgVal)labelStr);
            XtSetArg(args[4], XtNwidth, (XtArgVal)colrBtnWidth);
	    }
	XtSetValues(colrs[shift], args, XtNumber(args));
	XFlush(dpy);
        }
    if( numPages == 1)
	{
	XtSetArg(args[0], XtNsensitive, (XtArgVal)FALSE);
	XtSetValues(colrPgUp, args, (Cardinal)1);
	XtSetValues(colrPgDown, args, (Cardinal)1);
	}
    else
	{
	XtSetArg(args[0], XtNsensitive, (XtArgVal)TRUE);
	XtSetValues(colrPgUp, args, (Cardinal)1);
	XtSetValues(colrPgDown, args, (Cardinal)1);
	}
    /* now do the layout */
    XawFormDoLayout(XtParent(w), TRUE);
    /* release control (allow Chuck to work again) */
    (void)HideBusy(dpy, busy);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void PairsPageProc(w, client, call)
Args	 	:	    Widget w; -- the selected button
  	    		    XtPointer client; -- holds our editor Widget
  	    		    XtPointer call; -- UNUSED

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	15 March 1990
Last Revised	:
Abstract 	:	This is the callback for the page up and page down
			buttons for the pairs part of the prop sheet.
Returns  	:	None.

***********************************************************************/
static void PairsPageProc(w, client, call)
    Widget w;
    XtPointer client; /* hold our editor widget */
    XtPointer call;  /* UNUSED */
    {
    CellEditWidget cew = (CellEditWidget)client;
    Display *dpy = XtDisplay(w);
    int screen = DefaultScreen(dpy);
    char *pairsString;
    int done = 0;
    int start = 0;
    char label[80];
    unsigned long pixel;
    unsigned long labelPixel;
    int shift;
    int numLabels = DATA_WIDGETS;
    register int i;
    Arg args[5];
    int longest = strlen(pairsTitleStr);
    Widget long_widget = pairsTitle;
    int numPages;

    /* how many pages are there? */
    numPages = (int)ceil((double)cew->celledit.numData / (double)DATA_WIDGETS);

    if ((numPages == 1) && (pairsPgNum != 0))
	return;

    /* No Chucks! */
    (void)ShowBusy(dpy, busy);
    XawFormDoLayout(XtParent(w), FALSE);

    /* if PageDown */
    if (w == pairsPgDown)
	{
	/* increment page marker */
	pairsPgNum++;
	/* calc start and end points */
	if (( pairsPgNum > numPages) || (pairsPgNum == 0))
	    { /* wrap */
	    start = 0;
	    done = DATA_WIDGETS;
	    if ( done > cew->celledit.numData)
		numLabels = (cew->celledit.numData % DATA_WIDGETS);
	    pairsPgNum = 1;
	    }
	else
	    {
	    done = pairsPgNum * DATA_WIDGETS;
	    start = done - DATA_WIDGETS;
	    if ( done > cew->celledit.numData)
		numLabels = (cew->celledit.numData % DATA_WIDGETS);
	    }
        }
    else	/* else PageUp */
	{   
	pairsPgNum--;
	if (pairsPgNum == 0) /* go to last page */
	    { /* back wrap */
	    pairsPgNum = numPages;
	    done = (pairsPgNum) * DATA_WIDGETS;
	    start = done - DATA_WIDGETS;
	    if ( done > cew->celledit.numData) 
		numLabels = (cew->celledit.numData % DATA_WIDGETS);
	    }
	else
	    {
	    done = (pairsPgNum) * DATA_WIDGETS;
	    start = done - DATA_WIDGETS;
	    if ( done > cew->celledit.numData) 
		numLabels = (cew->celledit.numData % DATA_WIDGETS);
	    } 
	} 

    /* no redo the buttons */
    XtSetArg(args[0], XtNbackgroundPixmap, (XtArgVal)XtUnspecifiedPixmap);
    for( i = start, shift = 0; i < done; i++, shift++ )
	{
	/* Make sure we have data for these buttons */
	if ( shift < numLabels )
	    {
	    /* first get the color */
	    pixel = GetDataIndex(cew, cew->celledit.dataList[i]);
	    sprintf(label,"%d -- ",cew->celledit.dataList[i]);
	    /* the get the string */
	    free(pairsString);
	    pairsString =
		(char *)G_store((char *)G_get_cat(cew->celledit.dataList[i],
						 &(cew->celledit.cats)));
	    /* If there is no data name, use our string */
	    if ( streq(pairsString,"") )
		{
		strcat(label, NullNameString);
		strcat(label, "\0");
		}
	    else
		{
		strcat(label, pairsString);
		strcat(label, "\0");
		}
	    /* again, check for the longest */
	    /* for each data value, we want to create a command
	     * button with  it's label set to a foreground
	     * we can see, and it's backgound set to the proper,
	     * data color
	     */

	    labelPixel = CalcLabelColor(cew, pixel);

	    /* Now, create the buttons */
	    XtSetArg(args[1], XtNlabel, (XtArgVal)label);
	    XtSetArg(args[2], XtNforeground, (XtArgVal)labelPixel);
	    XtSetArg(args[3], XtNbackground, (XtArgVal)pixel);
	    XtSetArg(args[4], XtNsensitive, (XtArgVal)TRUE);
	    }
	else	/* we have no data for these */
	    {
	    XtSetArg(args[1], XtNlabel, (XtArgVal)NullDataString);
	    XtSetArg(args[2], XtNforeground, 
				    (XtArgVal)BlackPixel(dpy, screen));
	    XtSetArg(args[3], XtNbackground, 
				    (XtArgVal)WhitePixel(dpy, screen));
	    /* turn 'm off */
	    XtSetArg(args[4], XtNsensitive, (XtArgVal)FALSE);
	    }
	XtSetValues(pairs[shift], args, XtNumber(args));
	XFlush(dpy);
	if (strlen(label) > longest)
	    {
	    longest = strlen(label);
	    long_widget = pairs[shift];
	    }
	}
    /* if we got this far, pageup and page down should be set
     * to sensitive (we have more than one page), and we don't
     * know if they're sensitive or not
     */
    if (numPages == 1)
	{
	XtSetArg(args[0], XtNsensitive, (XtArgVal)FALSE);
	XtSetValues(pairsPgUp, args, (Cardinal)1);
	XtSetValues(pairsPgDown, args, (Cardinal)1);
	}
    else
	{
	XtSetArg(args[0], XtNsensitive, (XtArgVal)TRUE);
	XtSetValues(pairsPgUp, args, (Cardinal)1);
	XtSetValues(pairsPgDown, args, (Cardinal)1);
	}

    StretchButtons(long_widget);
    XawFormDoLayout(XtParent(w), TRUE);
    (void)HideBusy(dpy, busy);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void SetCurrColr(w, color)
Args	 	:	    Widget w; -- Our widget
  	    		    unsigned short color; -- New current pixel

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:
Abstract 	:	Sets a color into the current state section of the
			PS.
Returns  	:	None.

***********************************************************************/
static void SetCurrColr(w, color)
    Widget w;
    unsigned short color;
    {
    CellEditWidget cew = (CellEditWidget)w;
    Display *dpy = XtDisplay(cew);
    int screen = DefaultScreen(dpy);
    Arg args[3];
    char clrStr[80];
    char clrVal[14];    /* this should be plenty of room */
    unsigned short bgPixel = cew->celledit.pixelList[color];
    unsigned short fgPixel = CalcLabelColor(cew, bgPixel);

    G_strcpy(clrStr," Current Color:");
    sprintf(clrVal, " %ld ", color);
    strcat(clrStr, clrVal);

    XtSetArg(args[0], XtNlabel, (XtArgVal)clrStr);
    XtSetArg(args[1], XtNbackground, (XtArgVal)bgPixel);
    XtSetArg(args[2], XtNforeground, (XtArgVal)fgPixel);
    XtSetValues(currClrLbl, args, (Cardinal)3);
    currColor = bgPixel;
    XFlush(dpy);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void SetCurrData(w, data)
Args	 	:	    Widget w; -- Our widget
  	    		    int data; -- data value

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	17 March 1990
Last Revised	:
Abstract 	:	Sets a data value into the current state section of 
			the PS.
Returns  	:	None.

***********************************************************************/
static void SetCurrData(w, data)
    Widget w;
    int data;
    {
    CellEditWidget cew = (CellEditWidget)w;
    Display *dpy = XtDisplay(cew);
    int screen = DefaultScreen(dpy);
    Arg args[3];
    char dtaStr[80];
    char dtaVal[14]; /* this should be plenty */
    CELL dataVal = cew->celledit.dataList[data];

    G_strcpy(dtaStr," Current Data:");
    sprintf(dtaVal, " %ld ", dataVal);
    strcat(dtaStr, dtaVal);
    XtSetArg(args[0], XtNlabel, (XtArgVal)dtaStr);
    XtSetArg(args[1], XtNbackground, (XtArgVal)WhitePixel(dpy, screen));
    XtSetArg(args[2], XtNforeground, (XtArgVal)BlackPixel(dpy, screen));
    XtSetValues(currDtaLbl, args, (Cardinal)3);
    currData = dataVal;
    XFlush(dpy);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void UpdateState()

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	16 March 1990
Last Revised	:
Abstract 	:	Make sure the current state section is aligned
			and pretty.
Returns  	:	None.

***********************************************************************/
static void UpdateState()
    {
    int width;
    int longest;
    Arg arg[1];

    XtSetArg(arg[0], XtNwidth, &width);
    XtGetValues(stateTitle, arg, (Cardinal)1);
    longest = width;
    XtGetValues(currClrLbl, arg, (Cardinal)1);
    if (width > longest)
	longest = width;
    XtGetValues(currDtaLbl, arg, (Cardinal)1);
    if (width > longest)
	longest = width;
    XtSetArg(arg[0], XtNwidth, longest);
    XtSetValues(stateTitle, arg, (Cardinal)1);
    XtSetValues(currClrLbl, arg, (Cardinal)1);
    XtSetValues(currDtaLbl, arg, (Cardinal)1);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void BuildStatePart(w)
Args	 	:	    Widget w; -- Our widget

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	23 March 1990
Last Revised	:
Abstract 	:	Build the "current state" section of the 
			property sheet.
Returns  	:	None.

***********************************************************************/
static void BuildStatePart(w)
    Widget w;
    {
    CellEditWidget cew = (CellEditWidget)w;
    Display *dpy = XtDisplay(cew);
    int screen = DefaultScreen(dpy);
    unsigned short fgColor, bgColor;
    static char *defColor = " Current Color: 0 ";
    char defData[80];
    char curData[14]; /* This should be plenty of space */
    Arg getArg[1];
    Dimension width;


    /* Okay, start from the top */
    /* the first widget is this section's form */
    /* put it below the lists, on the left */
    stateForm = 
	XtVaCreateManagedWidget("stateForm", formWidgetClass, propForm,
				XtNfromVert, (XtArgVal)pairsForm, NULL);

    /* next make the title label */
    stateTitle = 
	XtVaCreateManagedWidget("stateTitle", labelWidgetClass, stateForm,
			      XtNlabel, (XtArgVal)stateTitleStr,
			      XtNbackgroundPixmap, (XtArgVal)backPix,
			      XtNleft, (XtArgVal)XtChainLeft,
			      XtNright, (XtArgVal)XtChainRight, NULL);

    /* next make the current color label */
    /* color 0 for now */
    bgColor = cew->celledit.pixelList[0];
    fgColor = CalcLabelColor(cew, bgColor);

    /* set the static value */
    currColor = cew->celledit.pixelList[0];

    currClrLbl = 
	XtVaCreateManagedWidget("currClrLbl", labelWidgetClass, stateForm,
				XtNlabel, (XtArgVal)defColor,
				XtNfromVert, (XtArgVal)stateTitle,
				XtNbackground, (XtArgVal)bgColor,
				XtNforeground, (XtArgVal)fgColor, NULL);

    /* next make the current data label */
    /* the first data value for now */
    G_strcpy(defData," Current Data:");
    sprintf(curData, " %ld ", (long)cew->celledit.dataList[0]);
    strcat(defData, curData);
    /* set the static value */
    currData = cew->celledit.dataList[0];

    /* now the current data value section */
    currDtaLbl = 
	XtVaCreateManagedWidget("currDtaLbl", labelWidgetClass, stateForm,
			    XtNlabel, (XtArgVal)defData,
			    XtNfromVert, (XtArgVal)currClrLbl,
			    XtNforeground, (XtArgVal)BlackPixel(dpy,screen),
			    XtNbackground, (XtArgVal)WhitePixel(dpy, screen),
			    NULL);

    /* our longest widget should be the title, so get it's width */
    XtSetArg(getArg[0], XtNwidth, &width);
    XtGetValues(stateTitle, getArg, (Cardinal)1);

    XtSetArg(getArg[0], XtNwidth, width);
    XtSetValues(currClrLbl, getArg, (Cardinal)1);
    XtSetValues(currDtaLbl, getArg, (Cardinal)1);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	BuildBrushPart(w)
Args	 	:	    Widget w; -- Our widget

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	24 March 1990
Last Revised	:
Abstract 	:	build the current brush section of the PS
Returns  	:	None.

***********************************************************************/
static void BuildBrushPart(w)
    Widget w;
    {
    CellEditWidget cew = (CellEditWidget)w;
    Display *dpy = XtDisplay(cew);
    int screen = DefaultScreen(dpy);
    register int row, i, j;
    char name[80];
    char **usrBList;
    char **defBList;
    char userBrushPath[120];
    char defBrushFullPath[120];
    Arg arg[1];
    int	defCount = 0;
    int	usrCount = 0;
    char tmp[40];

    /* first load the default brush */
    /* DEFBRUSHDIR is the location of the default brushes */	
#ifndef DEFBRUSHDIR
    fprintf(stderr, "Unable to find default brush directory\n");
    fprintf(stderr, "See README for compile time defines\n");
#else
    sprintf(defBrushFullPath, "%s/%s", DEFBRUSHDIR, defBrush);
    /* ./lib/brush/brushio.c */
    if (!ReadBrush(defBrushFullPath, &theBrush))
	{
	fprintf(stderr, "Unable to find default brush directory\n");
	fprintf(stderr, "Punting...\n");
	Destroy(cew, (XtPointer)NULL, (XtPointer)NULL);
	exit(-1);
	}
#endif

    /* the first widget is the brushForm */
    brushForm = 
	XtVaCreateManagedWidget("brushForm", formWidgetClass, propForm,
				XtNresizable, (XtArgVal)True,
				NULL);
    /* next the title */
    brushTitle =
	XtVaCreateManagedWidget("brushTitle", labelWidgetClass, brushForm,
				XtNlabel, (XtArgVal)brushTitleStr,
				XtNbackgroundPixmap, (XtArgVal)backPix,
				NULL);
    /* next the brush name */
    sprintf(name,"%s: %s", "Name", theBrush.name);
    brushName =
	    XtVaCreateManagedWidget("brushName", labelWidgetClass, brushForm,
				    XtNlabel, (XtArgVal) name,
				    NULL);
    /* the for that holds both brush parts (value, and function) */
    brushPart = 
	XtVaCreateManagedWidget("brushPart", formWidgetClass, brushForm,
				XtNresizable, (XtArgVal)True,
				NULL);
    /* the for holding the value section of the brush */
    valForm =
	XtVaCreateManagedWidget("valForm", formWidgetClass, brushPart,
				XtNresizable, (XtArgVal)True,
				NULL);
    /* the for holding the function section of the brush */
    funcForm = 
	XtVaCreateManagedWidget("funcForm", formWidgetClass, brushPart,
				XtNresizable, (XtArgVal)True,
				NULL);

    /* get memory for the label widgets */
    bVals = 
	(Widget **)G_calloc(theBrush.height, sizeof(Widget *));
    for (row = 0; row < theBrush.height; row++)
	{
	bVals[row] = 
	    (Widget *)calloc(theBrush.width, sizeof(Widget));
        }
    bFuncs = 
	(Widget **)G_calloc(theBrush.height, sizeof(Widget *));
    for (row = 0; row < theBrush.height; row++)
	{
	bFuncs[row] = 
	    (Widget *)calloc(theBrush.width, sizeof(Widget));
        }
    /* build the boxes */
    BuildBox(valForm, bVals, 0);
    BuildBox(funcForm, bFuncs, 1);

    /* now build the load brush menu */
    /* create "load brush" pulldown menuBtn */
    brushBtn =  (Widget)CreateMenuButton(brushForm, "brushBtn","brushBtnShell");
    XtSetArg(arg[0], XtNbackgroundPixmap, (XtArgVal)backPix);
    XtSetValues(brushBtn, arg, (Cardinal)1);
    brushBtnShell = (Widget)CreatePopupShell(brushBtn, "brushBtnShell", 1);

    /* get dir listing of default brushes */
    defBList = (char **)MakeFileList(DEFBRUSHDIR, &defCount);
    brushes = (Widget *)G_calloc(defCount+1, sizeof(Widget));
    for(i = 0; i < defCount ; i++)
	{
	brushes[i] = CreateBSB(brushBtnShell, (char *)defBList[i]);
	XtAddCallback(brushes[i], XtNcallback, LoadBrush, (XtPointer)1);
	}
    brushes[++i] = CreateLine(brushBtnShell,"line");
    sprintf(userBrushPath, "%s/%s/%s",
	G_location_path(), G_whoami(), "celledit/brushes");
    /* get dir listing of user's brushes */
    usrBList = (char **)MakeFileList(userBrushPath, &usrCount);
    brushes = 
	(Widget *)G_realloc((char *)brushes,
			(unsigned)((defCount+1+usrCount)*sizeof(Widget)));
    if (defCount)
	{
	for (i = (defCount + 1); i < (defCount + 1 + usrCount); i++)
	    {
	    brushes[i] = 
		CreateBSB(brushBtnShell,(char *)usrBList[i-(defCount+1)]);
	    XtAddCallback(brushes[i], XtNcallback, LoadBrush, (XtPointer)2);
	    }
	}
    }


/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void BuildBox(parent, children, which)
Args	 	:	    Widget parent; -- the boxes parent
  	    		    Widget **children; -- matrix of children
			    int which; -- which box (value of function).

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	28 March 1990
Last Revised	:
Abstract 	:	Create a form widget that displays an appropriate
			matrix of brush values of functions.
Returns  	:	None.

***********************************************************************/
static void BuildBox(parent, children, which)
    Widget parent;
    Widget **children;
    int which;
    {
    register int row = 0;
    register int col = 0;
    Arg args[3];
    char data[20];

    for ( row = 0; row < theBrush.height; row++)
	{ /* row by row */
	for ( col = 0; col < theBrush.height; col++)
	    { /* col by col */
	    if (row == 0)
		XtSetArg(args[0], XtNfromVert, (XtArgVal)NULL);
	    else
		XtSetArg(args[0], XtNfromVert, 
			(XtArgVal)children[row-1][col]);
	    if (col == 0)
		XtSetArg(args[1], XtNfromHoriz, (XtArgVal)NULL);
	    else
		XtSetArg(args[1], XtNfromHoriz, 
			(XtArgVal)children[row][col-1]);

	    if (which)
		{
		G_strcpy(data, functions[theBrush.funcs[row][col]]);
		XtSetArg(args[2], XtNlabel, (XtArgVal)data);
		}
	    else if (theBrush.vals[row][col] == CUR)
		XtSetArg(args[2], XtNlabel, (XtArgVal)"CUR");
	    else
		{
		sprintf(data, "%ld", theBrush.vals[row][col]);
		XtSetArg(args[2], XtNlabel, (XtArgVal)data);
		}

	    children[row][col] = 
		XtCreateManagedWidget("label", labelWidgetClass, parent,
					args, XtNumber(args));
	    XtManageChild(children[row][col]);
	    }
	}
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void LoadBrush(w, call, client)
Args	 	:	    Widget w; -- the menu entry that was hit
  	    		    XtPointer call; -- which brush directory
  	    		    XtPointer client; -- Unused

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	19 March 1990
Last Revised	:
Abstract 	:	loads a new brush into the current brush section.
Returns  	:	None.

***********************************************************************/
static void LoadBrush(w, client, call)
    Widget w;		/* one of the BSBs */
    XtPointer client; 	/* usr or def directory */
    XtPointer call;  	/* UNUSED */
    {
    register int row;
    Arg arg[4];
    char filename[120];
    char name [80];
    Display *dpy = XtDisplay(propForm);
    Widget tmpForm1;
    Widget tmpForm2;
    Widget **tmpVals1;
    Widget **tmpVals2;
    Dimension oldWidth, oldHeight, oldBw; 
    Dimension newWidth, newHeight, newBw; 
    Dimension parWidth, parHeight, parBw; 
    Dimension newW, newH, new_retW, new_retH; 
    static int i;

    /* No Chucks */
    ShowBusy(dpy, busy);

    /* build the right file name path */
    if ((int)client == 1)
	sprintf(filename,"%s/%s%c", DEFBRUSHDIR, XtName(w), '\0');
    else
	sprintf(filename, "%s/%s/%s/%s%c", G_location_path(), G_whoami(), 
				       "celledit/brushes", XtName(w), '\0');
    if (!ReadBrush(filename, &theBrush))
	G_warning("Unable to load brush\n");

    /* prevent relayouts till we're done */

    XawFormDoLayout(propForm, False);
    XawFormDoLayout(brushForm, False);
    XawFormDoLayout(brushPart, False);

    /* Set the new name */
    sprintf(name,"%s: %s", "Name", theBrush.name);
    XtSetArg(arg[0], XtNlabel,  name);
    XtSetValues(brushName, arg, (Cardinal)1);

    /* get old info */
    XtSetArg(arg[0], XtNwidth, &oldWidth);
    XtSetArg(arg[1], XtNheight, &oldHeight);
    XtSetArg(arg[2], XtNborderWidth, &oldBw);
    XtGetValues(brushPart, arg, (Cardinal)3);

    /* get parent info */
    XtSetArg(arg[0], XtNwidth, &parWidth);
    XtSetArg(arg[1], XtNheight, &parHeight);
    XtGetValues(brushForm, arg, (Cardinal)2);

    /* swap the pointers, and destroy the old ones */
    tmpForm1 = valForm;
    tmpForm2 = funcForm;

    /* unrealize the parent */
    XtUnrealizeWidget(brushPart);
    XtDestroyWidget(tmpForm1);
    XtDestroyWidget(tmpForm2);
    XSync(dpy, False);

    /* reset the old pointers */
    valForm = (Widget)NULL;
    funcForm = (Widget)NULL;
    *bVals = (Widget *)NULL;
    bVals = (Widget **)NULL;
    *bFuncs = (Widget *)NULL;
    bFuncs = (Widget **)NULL;

    /* reuse the new forms */
    /* create new forms */
    if (!(i%2))
	{
	valForm = (Widget)CreateForm(brushPart, "valForm");
	funcForm = 
	    (Widget)XtVaCreateManagedWidget("funcForm", formWidgetClass,
				 brushPart, XtNfromHoriz, (XtArgVal)valForm, 
				 NULL);
	}
    else
	{
	valForm = (Widget)CreateForm(brushPart, "valForm1");
	funcForm = 
	    (Widget)XtVaCreateManagedWidget("funcForm1", formWidgetClass,
				 brushPart, XtNfromHoriz, (XtArgVal)valForm, 
				 NULL);
	}

    /* allocate memory */

    tmpVals1 = 
	(Widget **)G_calloc(theBrush.height, sizeof(Widget *));
    for (row = 0; row < theBrush.height; row++)
	{
	tmpVals1[row] = 
	    (Widget *)G_calloc(theBrush.width, sizeof(Widget));
	}
    tmpVals2 = 
	(Widget **)G_calloc(theBrush.height, sizeof(Widget *));
    for (row = 0; row < theBrush.height; row++)
	{
	tmpVals2[row] = 
	    (Widget *)G_calloc(theBrush.width, sizeof(Widget));
	}
    /* build new boxes */
    BuildBox(valForm, tmpVals1, 0);
    BuildBox(funcForm, tmpVals2, 1);

    /* manage them */
    XtRealizeWidget(brushPart);
    XtManageChild(brushPart);
    XtMapWidget(brushPart);
    XSync(dpy, False);
    /* try to resize  this should work because brushPart is resizable */
    XtSetArg(arg[0], XtNwidth, &newWidth);
    XtSetArg(arg[1], XtNheight, &newHeight);
    XtSetArg(arg[2], XtNborderWidth, &newBw);
    XtGetValues(brushPart, arg, (Cardinal)3);

    newW = parWidth - (oldWidth +(2*oldBw)) + (newWidth + (2*newBw));
    newH = parHeight - (oldHeight +(2*oldBw)) + (newHeight + (2*newBw));

    /* ask for a resize */
    XtMakeResizeRequest(brushForm, newW, newH, &new_retW, &new_retH);

    /* Now do the layout */
    XawFormDoLayout(brushPart, True);
    XawFormDoLayout(brushForm, True);
    XawFormDoLayout(propForm, True);
    XSync(dpy, False);

    /* let Chuck at it. */
    HideBusy(dpy, busy);

    /* reset the pointers */
    bVals = tmpVals1;
    bFuncs = tmpVals2;
    }

/* PUBLIC FUNCTIONS ****************************************************/

/***********************************************************************

File     	:	CellEdit.c
Function 	:	void LoadCellFile(w, cellFil)
Args	 	:	    Widget w; -- Our widget
  	    		    char *cellFile; -- file name

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:	23 February 1990
Abstract 	:	Public function to load an cell file
Returns  	:	None.

***********************************************************************/

void LoadCellFile(w, cellFile)
    Widget w;
    char *cellFile;
    {
    CellEditWidget cew = (CellEditWidget)w;
    char *fileName;

    /* This function takes for ever, 
     * look for ways to optimize 
     */

    if (loaded)
        {
        /* this just gets unmapped then
         * reset via page proc. 
         */
        XtUnmapWidget(cew->celledit.propShell);
        XFlush(XtDisplay(cew));
	}


    /* clear the window */
    XClearWindow(XtDisplay(cew), XtWindow(cew));
    XFlush(XtDisplay(cew));

    /* loadthe new cellFileName name into the widget */
    cew->celledit.cellFileName = G_store(cellFile);
    cew->celledit.saveFileName = G_store(cellFile);


    /* now load the grass info */
    LoadGrassInfo(cew);

    /* try to resize */
    SetSize(cew);

    /* Translate Grass colors to X Colors, and load'm up*/
    SetColors(cew);

    /* Get all the data values out of the cellfile */
    GetDataVals(cew->celledit.cellFileName, cew->celledit.mapset,
		&(cew->celledit.dataList), &(cew->celledit.numData));

    /* Load/Draw the pixmap */
    LoadPixmap(cew);

    /* copy the image to the screen */
    XCopyArea(XtDisplay(cew), cew->celledit.image_backup, XtWindow(cew),
	      cew->celledit.gc, 0, 0, cew->core.width, cew->core.height,
	      0, 0);

    /* if this is the first time through we need to build the entire
     * property sheet 
     */
    if (!loaded)
	{
	/* build all the widgets */
	BuildPropSheet(cew);
	loaded = TRUE;
	}
    else /* else just update the existing sheet */
	{
	int status;
	G_free_cats(&(cew->celledit.cats));
	status =  G_read_cats(cew->celledit.cellFileName, cew->celledit.mapset,
			      &(cew->celledit.cats));
	if (status < 0)
	   G_warning("CellEditor Unable to read categories file\n");
	/* reset the page counters */
        pairsPgNum = 0;
        colrPgNum = 0;
	/* call the page procs with these new values */
        PairsPageProc(pairsPgDown, (XtPointer)cew, (XtPointer)NULL);
        ColrPageProc(colrPgDown, (XtPointer)cew, (XtPointer)NULL);
	/* update the current state section */
	SetCurrColr(cew, 0);
	SetCurrData(cew, 0);
	UpdateState();
	/* remap the PS */
        XtMapWidget(cew->celledit.propShell);
        }
    imageW = cew->celledit.cols * cew->celledit.cell_res;
    imageH = cew->celledit.rows * cew->celledit.cell_res;
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	Status SetSaveFile(w, filename,)
Args	 	:	    Widget w; -- Our widget
  	    		    char * filename; -- Proposed file name

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:	23 February 1990
Abstract 	:	Public function to test the filename, and if good 
			store it as the output file name.
Returns  	:	1  if the filename is good,
			0  if no cell file has been loaded yet.
			-1 if the filename is  bad

***********************************************************************/
Status SetSaveFile(w, fileName)
    Widget w;
    char *fileName;
    {
    CellEditWidget cew = (CellEditWidget)w;
    char *firstChar = fileName;
    char *newFileName = fileName;
    /* set the saveFileName apporpriately */ 

    /* if the user hasn't loaded a cellFileName yet,
     * then we can't save to file
     */
    if (cew->celledit.saveFileName == NULL)
       return (0);
    /* filename can be null, which means use the old name,
     * but we need to  check if they've loaded
     * a cell file yet, so you still must call this.
     */
    else if (fileName == NULL)
	return (1);
    else if (G_legal_filename(newFileName) == -1 ) 
	return(-1);

    cew->celledit.saveFileName = G_store(newFileName);
    return(1);
    } 

/***********************************************************************

File     	:	CellEdit.c
Function 	:	void SaveCellFile(w)
Args	 	:	    Widget w; -- Our widget

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:	23 February 1990
Abstract 	:	Public function to save our work. This is not
			presumed to work yet.
Returns  	:	None.

***********************************************************************/
void SaveCellFile(w)
    Widget w;
    {
    CellEditWidget cew = (CellEditWidget)w;
    register int row;
    CELL *buf = (CELL *)G_allocate_cell_buf();
    int saveFileDesc;

    /* Open the save file */
    saveFileDesc = G_open_cell_new(cew->celledit.saveFileName);
    if (saveFileDesc < 0)
	G_fatal_error("Unable to open save file \n");


    /* loop through the rows */
    for (row = 0; row < cew->celledit.rows; row++)
	{
	/* Flush the segment (just following the manual) */
	segment_flush(&(cew->celledit.segment));
	/* read it in, */
	if( segment_get_row(&(cew->celledit.segment), buf, row) < 0 )
	    G_fatal_error("Unable to get segment row \n");
	/* and write it out */
	if(G_put_map_row(saveFileDesc, buf) < 0 )
	    G_fatal_error("Unable to write row\n");
	}

    /* free memory, and close file */
    free(buf);

    /* NOTE we don't close the segment file until we exit */
    G_close_cell(saveFileDesc);

    /* Give the file our colormap */

    /* we have the cellfile's colors in RAM (hog)
     * so write them out 
     */
    G_write_colors(cew->celledit.saveFileName, cew->celledit.mapset, 
		   &(cew->celledit.colors));
    /* we free the grass colors when we exit */
    
    /* This is not the right way to save a file. Some thought needs
     * to be put into the appropriate way to write this out so that it
     * can be merged into the correct file as it is not an image of the
     * entire file.
     */
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static unsigned short GetDataIndex(w, value)
Args	 	:	    Widget w; -- Our widget
  	    		    CELL value; -- a data value

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:	23 February 1990
Abstract 	:	Public function to find the appropriate 
			index into the X color map
Returns  	:	the pixel(color) with which to draw this data.

***********************************************************************/
#define OFFSET(x) ((long)(x) - (long)cew->celledit.min_color)
static unsigned short GetDataIndex(w, value)
    Widget w;
    CELL value;
    {
    CellEditWidget cew = (CellEditWidget)w;

    if ( (long)value == (long)0)
	{
	return((unsigned short)cew->celledit.pixelList[0]);
	}
    else
	{
	int offset = OFFSET(value);
	offset++;
	return((unsigned short)cew->celledit.pixelList[offset]);
	}
    }


/***********************************************************************

File     	:	CellEdit.c
Function 	:	char *GetCellPath(w)
Args	 	:	    Widget w; -- Our widget

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	: 	23 February 1990
Abstract 	:	returns the path to the cell files
Returns  	:	the path.

***********************************************************************/
char *GetCellPath(w)
    Widget w;
    {
    CellEditWidget cew = (CellEditWidget)w;
    return(cew->celledit.cellFilePath);
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	static unsigned short CalcLabelColor(w, bgColor)
Args	 	:	    Widget w; -- Our widget
  	    		    unsigned short bgColor; -- bg color

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:
Abstract 	:	Given a bg color, this routines does a quick
			intensity calculation and retruns a suitable
			fg color that will be visible. (black or white).

Returns  	:	black pixel, or white pixel.

***********************************************************************/
static unsigned short CalcLabelColor(w, bgColor)
    Widget w;
    unsigned short bgColor;
    {
    CellEditWidget cew = (CellEditWidget)w;
    Display *dpy = XtDisplay(cew);
    int screen = DefaultScreen(dpy);
    XColor color;
    double intensity;

    /* first query the color */
    color.pixel = bgColor;
    XQueryColor(dpy, cew->core.colormap, &color);

    /* This is purely an estimation, no guarantees this will
     * work on any other color systems. If intensity is less
     * than %50, it gets a white label, otherwise black. It seems
     * to work pretty well though.
     */
    intensity = ((.30 * (float)color.red) +
		 (.59 * (float)color.green) +
		 (.11 * (float)color.blue));
    return ((intensity <= 32768.0) ?
                    WhitePixel(dpy, screen) : BlackPixel(dpy, screen));

     }

/* debugging routines */
PrintData(w)
    Widget w;
    {
    CellEditWidget cew = (CellEditWidget)w;
    register int i;
    for (i = 0; i < cew->celledit.numData; i++)
	{
	printf("data[%d] = %d\n", i, cew->celledit.dataList[i]);
	}
    }

PrintColors(w)
    Widget w;
    {
    CellEditWidget cew = (CellEditWidget)w;
    register int i;

    for (i = 0; i < cew->celledit.numPixels; i++)
	{
	printf("Pixel[%d] = %d\n", i, cew->celledit.pixelList[i]);
	}
     }
/***********************************************************************

File     	:	CellEdit.c
Function 	:	static void Edit(w, event,)
Args	 	:	    Widget w; -- Our widget
  	    		    XEvent event; -- Button 1 Up event

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	23 February 1990
Last Revised	:
Abstract 	:	This is the edit callback. It doesn't do much yet. 
Returns  	:	none.

***********************************************************************/
static void Edit(w,event)
    Widget w;
    XEvent *event;
    {
    CellEditWidget cew = (CellEditWidget)w;
    register Display *dpy = XtDisplay(cew);
    register int rowNum, colNum;
    CELL value;
    unsigned long pixel;
    /* where the button hit occured */
    int x = event->xbutton.x;	
    int y = event->xbutton.y;
    int delta = cew->celledit.cell_res;
    unsigned int bgColor, fgColor;

    /* which column and row */
    int col = (int)floor( ((double)x / (double)delta));
    int row = (int)floor( ((double)y / (double)delta));

    /* the dimensions of the image itself */
    int width = cew->celledit.cols * delta;
    int height = cew->celledit.rows * delta;

    /* if it is outside our image, do nothing */
    if ( ( (col * delta)  > width ) || ( (row * delta) > height ) )
	return;

    printf("Trying to edit\n");
    /* figure the edit */
    for (rowNum = 0; rowNum < theBrush.height; rowNum++, row++)
	{
	for (colNum = 0; colNum < theBrush.width; colNum++, col++)
	    {
	    switch(theBrush.funcs[rowNum][colNum])
		{
		case NIL:
		    break;
	        case CPY:
		    {
		    int ptX = col * delta;
		    int ptY = row * delta;
		    value = theBrush.vals[rowNum][colNum];
		    if (value = CUR)
			{
		   	bgColor = currColor;
			fgColor = CalcLabelColor(cew, bgColor);
			XSetForeground(dpy, cew->celledit.gc,
				       bgColor);

			printf("Filling %d,%d, to %d,%d\n",
			   (int)ptX, (int)ptY,
                           (unsigned int)(ptX + delta), 
			   (unsigned int)(ptY + delta));

			/* draw to the backup */
			XFillRectangle(dpy, cew->celledit.image_backup, 
			   cew->celledit.gc, 
			   (int)ptX, (int)ptY,
                           (unsigned int)delta, 
			   (unsigned int)delta);
			/* draw to the window */
			XFillRectangle(dpy, XtWindow(cew), cew->celledit.gc,
			   (int)ptX, (int)ptY,
                           (unsigned int)(delta),
			   (unsigned int)(delta));

			/* mark the cells we edited 
			 * only on the window.
			 */
			if (cew->celledit.cell_res >= 3 )
			    {
			    XSetLineAttributes(dpy, cew->celledit.gc,
				(unsigned int)1, LineSolid, CapButt, 
				JoinMiter);
			    XSetForeground(dpy, cew->celledit.gc,
					   fgColor);
			    XDrawLine(dpy, XtWindow(cew), cew->celledit.gc,
				ptX, ptY, ptX + delta, ptY + delta );
			    XDrawLine(dpy, XtWindow(cew), cew->celledit.gc,
				ptX, ptY + delta -1, ptX + delta, ptY -1);
			    }

			/* update segment file */
			segment_put(&(cew->celledit.segment), 
				    (char *)&currData, row, col);
			}
		    else
			{
			/* Handle other case */
			}
		    break;
		    }
		}
	    }
	col -= colNum; /* reset the column marker */
	}
    }

/***********************************************************************

File     	:	CellEdit.c
Function 	:	Refresh(w, event)
Args	 	:	    Widget w; -- Our Widget
  	    		    XEvent event; -- The event

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	08 May 1990
Last Revised	:
Abstract 	:	Send Query Event
Returns  	:	None.

***********************************************************************/
static void Refresh (w,event)
    Widget w;
    XEvent *event;
    {
    CellEditWidget cew = (CellEditWidget)w;

    XCopyArea(XtDisplay(cew), cew->celledit.image_backup, XtWindow(cew),
	      cew->celledit.gc, 0, 0, cew->core.width, cew->core.height, 0, 0);
    }

void GetImageExtents(width, height)
    int *width;
    int *height;
    {
    *width = imageW;
    *height = imageH;
    }

void QueryCellFile(w, x,y, row, col, value)
    Widget w;
    int x, y, *row, *col, *value;
    {
    CellEditWidget cew = (CellEditWidget)w;
    int delta = cew->celledit.cell_res;
    CELL val;

    *col = (int)floor( ((double)x / (double)delta));
    *row = (int)floor( ((double)y / (double)delta));
    segment_get(&(cew->celledit.segment), (char *)&val, *row, *col);
    *value = (int)val;
    }
