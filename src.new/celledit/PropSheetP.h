/***********************************************************************

File     	:	PropSheetP.h

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	25 February 1990
Last Revised	:	15 March 1990	-- spanki@ced.berkeley.edu
Abstract 	:	This file holds all the static information
			required by the celleditor widget to build and 
			maintain the property sheet (which is one of it's
			children). I felt the addition of this
			file, would help others follow the organization
			of the CellEditor Widget and it's children

***********************************************************************/
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/bitmaps/gray3>
#include "./lib/brush.h"

/* The celleditor widget itself holds the 
 * topLevelShell for the property sheet 
 */
static Widget propForm;			/* the first widget under the shell */
static Window busy;
static char pageUpStr[]= "PAGE UP";	/* str used for paging */
static char pageDownStr[]= "PAGE DOWN"; /* str used for paging */

static Pixmap backPix; /* used on the control labels/btns */

/* The following declarations are for 
 * the  ---Preset Data Pairs Section --- 
 * of the property sheet
 */
#define DATA_WIDGETS 10

static Widget pairsForm;	/* form for data section of */
static Widget pairsTitle;	/* (label) title of data section */
static Widget pairs[10];	/* (button) The 10 visible data values */
static Widget pairsPgUp;	/* (button) page up the data values */
static Widget pairsPgDown;	/* (button) page down the data values */

/* label for data pair section */
static char pairsTitleStr[]= "<< SELECT A PRESET DATA PAIR >>";
static char NullNameString[] = "No Data Name";
static char NullDataString[] = "No Data Value";

static int pairsPgNum;
static Arg pairArgs[] = 
    {
/* 0 */ {XtNlabel, 		(XtArgVal)pairsTitleStr},
/* 1 */ {XtNborderWidth,	(XtArgVal)1},
/* 2 */ {XtNforeground, 	(XtArgVal)NULL},
/* 3 */ {XtNbackground, 	(XtArgVal)NULL},
/* 4 */ {XtNbackgroundPixmap, 	(XtArgVal)XtUnspecifiedPixmap},
/* 5 */ {XtNfromVert, 		(XtArgVal)NULL},
/* 6 */ {XtNfromHoriz, 		(XtArgVal)NULL},
/* 7 */ {XtNresizable, 		(XtArgVal)True},
/* 8 */ {XtNjustify, 		(XtArgVal)XtJustifyLeft},
/* 9 */ {XtNsensitive, 		(XtArgVal)TRUE},
/* 10 */ {XtNleft, 		(XtArgVal)XtChainLeft},
/* 11 */ {XtNright, 		(XtArgVal)XtChainRight}
    };

/* The following declarations are for 
 * the  ---Colr Part--- 
 * of the property sheet
 */

#define COLR_WIDGETS 	10
#define COLR_WIDTH 	60
#define COLR_HEIGHT 	20

static Widget colrForm;
static char colrTitleStr[]= "<< SELECT A COLOR >>";
static Widget colrTitle;
static Widget colrPgUp;
static Widget colrPgDown;
static Widget colrs[10];

static int colrPgNum;
static int colrBtnWidth;

static Arg colrArgs[] = 
    {
{XtNlabel, 		(XtArgVal)""},
{XtNborderWidth,	(XtArgVal)1},
{XtNforeground, 	(XtArgVal)XtDefaultForeground},
{XtNbackground, 	(XtArgVal)XtDefaultBackground},
{XtNbackgroundPixmap, 	(XtArgVal)XtUnspecifiedPixmap},
{XtNwidth, 		(XtArgVal)COLR_WIDTH},
{XtNheight, 		(XtArgVal)COLR_HEIGHT},
{XtNsensitive, 		(XtArgVal)TRUE},
{XtNfromVert, 		(XtArgVal)NULL},
{XtNfromHoriz, 		(XtArgVal)NULL},
{XtNresizable, 	(XtArgVal)True},
{XtNjustify, 		(XtArgVal)XtJustifyLeft},
{XtNleft, 		(XtArgVal)XtChainLeft},
{XtNright, 		(XtArgVal)XtChainRight}
};

/* Current State Section ************************************************/

static Widget stateForm; 	/*  the bounding form 	*/ 
static Widget stateTitle; 	/*  the title label */ 
static Widget currClrLbl;  /*  the currently selected label */
static Widget currDtaLbl;  /*  the currently selected data label */
static char   stateTitleStr[] = "<< CURRENT STATE >>";
static CELL   currData;		/* the current data value */
static unsigned short currColor; /* the current color */

/* Current Brush Section ************************************************/

static BRUSH theBrush;
static Widget brushForm;  /* bounding form for the brushes */
static Widget brushTitle; /* title of the current brush section */
static Widget brushName;  /* name of the current brush */
static Widget brushPart;  /* brush parts form */
static Widget valForm;	  /* the values section of the current Brush */
static Widget **bVals; 	  /* one label for each cell of the brush */
static Widget funcForm;	  /* the functions section of the current Brush */
static Widget **bFuncs;   /* one label for each cell of the brush */

/* the name of the default brush */
static char defBrush[] = "2x2";
static char brushTitleStr[] = "<< CURRENT BRUSH >>";
static char bValsTitleStr[] = "Data Values";
static char bFuncsTitleStr[] = "Functions";

static Widget brushBtn;		/* menu btn for loading brushes */
static Widget brushBtnShell;	/* menu shell for load brush menu */
static Widget *brushes;		/* bsb entries for each brush */
