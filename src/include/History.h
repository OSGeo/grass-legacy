/*
 * File: History.h
 *
 * Desc: Public interface for History widget
 *
 * Auth: Eric W. Sink
 *
 * Date: 24 Feb 1992
 *
 * Modification History:
 *
 *
 */

#ifndef _History_h
#define _History_h

#include <Xm/Xm.h>

/* Class record constants */

externalref WidgetClass         historyWidgetClass;

typedef struct _HistoryClassRec *HistoryWidgetClass;
typedef struct _HistoryRec     *HistoryWidget;

#ifndef XmHISTORY_BIT
#define XmHISTORY_BIT (55)
#endif

#ifndef XgIsHistory
#define XgIsHistory(w) (_XmIsFastSubclass(XtClass(w),XmHISTORY_BIT))
#endif

/* define XmNresources */

#define XmNhistory	"history"
#define XmCHistory	"History"
#define XmRHistoryStructPtr "struct History *"

/*
 * Creation entry points:
 */
#ifdef _NO_PROTO

extern                          Widget
                                XgHistoryGetChild();
extern                          Widget
                                XgCreateHistory();
extern                          Widget
                                XgCreateHistoryDialog();

#else                           /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
extern                          "C" {
#endif


    extern Widget                   XgHistoryGetChild(Widget fs, unsigned char which);
    extern Widget                   XgCreateHistory(Widget p, String name, ArgList args,
                                                                Cardinal n);
    extern Widget                   XgCreateHistoryDialog(Widget ds_p, String name,
                                          ArgList fsb_args, Cardinal fsb_n);
#endif                          /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
}

#endif


#endif                          /* _History_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
