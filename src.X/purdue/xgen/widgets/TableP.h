#ifndef _XmTableP_h
#define _XmTableP_h

#include <Xm/XmP.h>

#include "Table.h"

/******************************
 * Class Part and Class Record
 ******************************/
typedef struct {
    int nothing;
} XmTableClassPart;

typedef struct _XmTableClassRec {
    CoreClassPart        core_class;
    CompositeClassPart   composite_class;
    ConstraintClassPart  constraint_class;
    XmManagerClassPart   manager_class;
    XmTableClassPart     table_class;
} XmTableClassRec;

/************************************
 * Instance Part and Instance Record
 ************************************/
 typedef struct {
    /************
     * resources
     ************/
    /* XmNrows, XmNcolumns */
    int rows, columns;

    /* XmN[rows,columns]Displayed */
    int rowsDisplayed, colsDisplayed;
 
    /* XmNcolumnHeadings */
    XmString *colHeadings;
 
    /* XmNcrowHeadings */
    XmString *rowHeadings;
 
    /* XmNcolumnWidth */
    int colWidth;
 
    /* XmNrowHeight */
    int rowHeight;
 
    /* XmNtableMargin[Width,Height] */
    int marginWidth,marginHeight;
 
    /* XmNspacing, XmNheadingSpacing */
    int spacing, headingSpacing;
 
    /* XmN[heading,entry]FontList */
    XmFontList headingFontList, entryFontList;
 
    /* XmNtitleFontColor */
    Pixel titleFontColor;
 
    /* XmNcolumnHeadingFontColor */
    Pixel columnHeadingFontColor;
 
    /* XmNrowHeadingFontColor */
    Pixel rowHeadingFontColor;
 
    /* XmNseparator */
    char separator;
 
    /* XmN[horizontal,vertical]ScrollBar */
    Widget hScrollBar, vScrollBar;
 
    /****************
     * private state
     ****************/
    /* Positions/Dimensions of the scrollbars */
    Position vsbX, vsbY;
    Dimension vsbWidth, vsbHeight;
    Position hsbX, hsbY;
    Dimension hsbWidth, hsbHeight;
 
    /* Other important scrollbar info */
    int vMin, vMax, vOrigin, vExtent;
    int hMin, hMax, hOrigin, hExtent;
    Boolean hasVSB, hasHSB;

    /* Dimensions/Positions of the title, heading and table areas */
    int titleX, titleY, titleWidth, titleHeight;
    int headingX, headingY, headingWidth, headingHeight;
    int rowHeadingX, rowHeadingY, rowHeadingWidth, rowHeadingHeight;
    int tableX, tableY, tableWidth, tableHeight;

    /* private Widgets that make up the table */
    /* the title */
    Widget  title;
    /* the two frames for the clipping widgets */
    Widget  headingFrame, rowHeadingFrame, tableFrame;
    /* the two clipping widgets */
    Widget  headingWindow, rowHeadingWindow, tableWindow;
    /* the two row column widgets */
    Widget  headingRC, rowHeadingRC, tableRC;
    /* pointers to the column headings and the table elements */
    Widget *columnHeads;
    Widget *rowHeads;
    Widget *tableElements;
	Boolean hasColHeadings;
	Boolean hasRowHeadings;
    
 } XmTablePart;

typedef struct _XmTableRec {
    CorePart        core;
    CompositePart   composite;
    ConstraintPart  constraint;
    XmManagerPart   manager;
    XmTablePart     table;
} XmTableRec;

/* Miscellany */
#define XmTABLE_BIT 0

#endif /* _TableP_h */
/* DON'T ADD STUFF AFTER THIS #endif */
