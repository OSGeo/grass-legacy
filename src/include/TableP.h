/**********************************************************************
   TableP.h - private header file for table widget
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
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
