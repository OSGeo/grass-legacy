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
 * XmNtitleString         XmCTitleString         XmString         NULL
 * XmNcolumnHeadings      XmCColumnHeadings      XmStringTable    NULL
 * XmNrowHeadings         XmCRowHeadings         XmStringTable    NULL
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

