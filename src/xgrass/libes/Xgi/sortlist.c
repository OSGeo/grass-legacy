static char     rcsid[] = "@(#)XGRASS $Id: dirlist.c,v 0.0 1992/05/05 14:56:15 sink Exp sink $";
/*
 * File: dirlist.c
 * 
 * Desc: contains code for sorting an XmList contents
 * 
 * Auth: Kurt Buehler
 * 
 * Date: Thu Jul  2 08:32:53 CDT 1992
 * 
 * Modification History:
 * 
 * 
 */

#include "xgrass_lib.h"

void
#ifdef _NO_PROTO
XgSortListItems(widget, order)
Widget widget;
int order;
#else
XgSortListItems(Widget widget, int order)
#endif
{
    XmString *items;
    int nitems;
    char **array;

    XtVaGetValues(widget, XmNitems, &items, XmNitemCount, &nitems, NULL);
    array = (char **)_XgXmStringTable2StringArray(items, nitems);
    XmListDeleteAllItems(widget);
    _XgSortStringArray(array, nitems, order);
    _XgPutStringArrayInList(widget, array, nitems);
}
