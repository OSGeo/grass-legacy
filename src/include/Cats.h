/*
 * File: Cats.h
 *
 * Desc: Public interface for Cats widget
 *
 * Auth: Eric W. Sink
 *
 * Date: 24 Feb 1992
 *
 * Modification History:
 *
 *
 */

#ifndef _Cats_h
#define _Cats_h

#include <Xm/Xm.h>

/* Class record constants */

externalref WidgetClass         catsWidgetClass;

typedef struct _CatsClassRec *CatsWidgetClass;
typedef struct _CatsRec     *CatsWidget;

#ifndef XmCATS_BIT
#define XmCATS_BIT (56)
#endif

#ifndef XgIsCats
#define XgIsCats(w) (_XmIsFastSubclass(XtClass(w),XmCATS_BIT))
#endif

/* define XmNresources */

#define XmNcats	"cats"
#define XmCCats	"Cats"
#define XmRCatsStructPtr "struct Categories *"

#define XmNrange	"range"
#define XmCRange	"Range"
#define XmRRangeStructPtr "struct Range *"

/*
 * Creation entry points:
 */
#ifdef _NO_PROTO

extern                          Widget
                                XgCatsGetChild();
extern                          Widget
                                XgCreateCats();
extern                          Widget
                                XgCreateCatsDialog();

#else                           /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
extern                          "C" {
#endif


    extern Widget                   XgCatsGetChild(Widget fs, unsigned char which);
    extern Widget                   XgCreateCats(Widget p, String name, ArgList args,
                                                                Cardinal n);
    extern Widget                   XgCreateCatsDialog(Widget ds_p, String name,
                                          ArgList fsb_args, Cardinal fsb_n);
#endif                          /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
}

#endif


#endif                          /* _Cats_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
