/**********************************************************************
   Table.h - public header file for table widget
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
#ifndef _XmTable_h
#define _XmTable_h

#include <Xm/Xm.h>

/* Resources:
 *
 * Name                   Class                  RepType          Default Value
 * ----                   -----                  -------          -------------
 * (all Core, Composite, Constraint, and XmManager resources)
 *
 * XmNrows                XmCRows                unsigned int     1
 * XmNcolumns             XmCColumns             unsigned int     1
 * XmNrowsDisplayed       XmCRowsDisplayed       unsigned int     2
 * XmNcolumnsDisplayed    XmCColumnsDisplayed    unsigned int     2
 * XmNcolumnWidth         XmCColumnWidth         Dimension        dynamic
 * XmNrowHeight           XmCRowHeight           Dimension        dynamic
 * XmNheadingSpacing      XmCHeadingSpacing      Dimension        4
 * XmNspacing             XmCSpacing             Dimension        4
 * XmNmarginWidth         XmCMarginWidth         Dimension        0
 * XmNmarginHeight        XmCMarginHeight        Dimension        0
 * XmNentryFontList       XmCEntryFontList       FontList         fixed
 * XmNheadingFontList     XmCHeadingFontList     FontList         fixed
 * XmNtitleFontColor      XmCTitleFontColor      Pixel            black
 * XmNtitleString         XmCTitleString         XmString         NULL
 * XmNcolumnHeadings      XmCColumnHeadings      XmStringTable    NULL
 * XmNcolumnHeadingFontColor XmCTitleFontColor   Pixel            black
 * XmNrowHeadings         XmCRowHeadings         XmStringTable    NULL
 * XmNrowHeadingFontColor XmCTitleFontColor      Pixel            black
 * XmNseparator           XmCSeparator           char             ':'
 * XmNhorizontalScrollBar XmCHorizontalScrollBar Widget
 * XmNverticalScrollBar   XmCVerticalScrollBar   Widget
 */

/* Names and Classes for the resources (not already defined) */
#define XmNheadingSpacing   "headingSpacing"
#define XmCHeadingSpacing   "HeadingSpacing"

#define XmNentryFontList "entryFontList"
#define XmCEntryFontList "EntryFontList"

#define XmNheadingFontList "headingFontList"
#define XmCHeadingFontList "HeadingFontList"

#define XmNtitleFontColor "titleFontColor"
#define XmCTitleFontColor "TitleFontColor"

#define XmNcolumnHeadingFontColor "columnHeadingFontColor"
#define XmCColumnHeadingFontColor "ColumnHeadingFontColor"

#define XmNrowHeadingFontColor "rowHeadingFontColor"
#define XmCRowHeadingFontColor "RowHeadingFontColor"

#define XmNrowsDisplayed "rowsDisplayed"
#define XmCRowsDisplayed "RowsDisplayed"

#define XmNcolumnsDisplayed "columnsDisplayed"
#define XmCColumnsDisplayed "ColumnsDisplayed"

#define XmNcolumnHeadings "columnHeadings"
#define XmCColumnHeadings "ColumnHeadings"

#define XmNrowHeadings "rowHeadings"
#define XmCRowHeadings "RowHeadings"

#define XmNcolumnWidth "columnWidth"
#define XmCColumnWidth "ColumnWidth"

#define XmNrowHeight "rowHeight"
#define XmCRowHeight "RowHeight"

#define XmNseparator "separator"
#define XmCSeparator "Separator"

#ifndef XmNtitleString
#define XmNtitleString "titleString"
#endif

#ifndef XmCTitleString
#define XmCTitleString "TitleString"
#endif

/* API declarations */
extern Widget XmCreateTable();
extern char * XmTableGetValue();
extern void XmTableSetValue();
extern char * XmTableGetRow();
extern void XmTableSetRow();
extern char * XmTableGetColumn();
extern void XmTableSetColumn();

/* class record constants */
extern WidgetClass xmTableWidgetClass;

typedef struct _XmTableClassRec * XmTableWidgetClass;
typedef struct _XmTableRec      * XmTableWidget;

#endif /* _XmTable_h */
/* DON'T ADD STUFF AFTER THIS #endif */

