#include <X11/Intrinsic.h>

#define _XmChangeNavigationType(widget, navtype)\
    XtVaSetValues(widget, XmNnavigationType, navtype, NULL)
