/*
 * File: Region.h
 *
 * Desc: Public interface for Region widget
 *
 * Auth: Eric W. Sink
 *
 * Date: 24 Feb 1992
 *
 * Modification History:
 *
 *
 */

#ifndef _Region_h
#define _Region_h

#include <Xm/Xm.h>

#ifndef XmREGION_BIT
#define XmREGION_BIT (53)
#endif

/* Class record constants */

externalref WidgetClass         regionWidgetClass;

typedef struct _RegionClassRec *RegionWidgetClass;
typedef struct _RegionRec      *RegionWidget;


#ifndef XgIsRegion
#define XgIsRegion(w) (_XmIsFastSubclass(XtClass(w),XmREGION_BIT))
#endif

#define XmNeditDefaultRegion "editDefaultRegion"
#define XmCEditDefaultRegion "EditDefaultRegion"

#define XmNcurrentRegion "currentRegion"
#define XmCCurrentRegion "CurrentRegion"
#define XmRCellHeadPtr "struct Cell_head *"

#define XmNgridColor "gridColor"
#define XmCGridColor "GridColor"

#define XmNgrid "grid"
#define XmCGrid "Grid"

#define XG_GRID_NONE 0
#define XG_GRID_COORD 1
#define XG_GRID_RASTER 2
#define XG_GRID_CURRENT 3
#define XG_GRID_DEFAULT 4

/*
 * Creation entry points:
 */
#ifdef _NO_PROTO

extern Widget 
XgRegionGetChild();
extern Widget 
XgCreateRegion();
extern Widget 
XgCreateRegionDialog();

#else                           /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
extern                          "C" {
#endif


    extern Widget                   XgRegionGetChild(Widget fs, unsigned char which);
    extern Widget                   XgCreateRegion(Widget p, String name, ArgList args,
                                                                Cardinal n);
    extern Widget                   XgCreateRegionDialog(Widget ds_p, String name,
                                          ArgList fsb_args, Cardinal fsb_n);
#endif                          /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
}

#endif


#endif                          /* _Region_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
