static char     rcsid[] = "@(#)XGRASS $Id: stringarray.c,v 0.0 1992/05/05 14:56:23 sink Exp sink $";
/*
 * File: stringarray.c
 * 
 * Desc: contains code for dealing with arrays of strings
 * 
 * Auth: Kurt Buehler Eric W. Sink
 * 
 * Date: Tue Nov  5 16:21:47 CST 1991
 * 
 * Modification History:
 * 
 * 
 */

#include "xgrass_lib.h"

#ifdef _NO_PROTO
void
_XgSortStringArray(ptr, items, sort)
    char          **ptr;
    int             items;
    int             sort;
#else
void
_XgSortStringArray(char **ptr, int items, int sort)
#endif
{
    int             i;
    for (i = 0; i < items; i++) {
        int             j;

        for (j = 0; j < items - i - 1; j++) {
            if (sort == 1) {
                if (strcmp(ptr[j], ptr[j + 1]) > 0) {
                    char           *tmp = ptr[j];

                    ptr[j] = ptr[j + 1];
                    ptr[j + 1] = tmp;
                }
            } else if (sort == 2) {
                if (strcmp(ptr[j], ptr[j + 1]) < 0) {
                    char           *tmp = ptr[j];

                    ptr[j] = ptr[j + 1];
                    ptr[j + 1] = tmp;
                }
            }
        }
    }
}

char          **
#ifdef _NO_PROTO
_XgStringArrayDelete(ptr, items, deaditem)
    char          **ptr;
    int             items;
    char           *deaditem;
#else
_XgStringArrayDelete(char **ptr, int items, char *deaditem)
#endif
{
    int             i = 0;
    int             j;
    char          **result;
    result = (char **) _XgCalloc(items - 1, sizeof(char *));

    if (ptr && items) {
        i = 0;
        j = 0;
        for (i = 0; i < items; i++) {
            if (strcmp(deaditem, ptr[i]))
                result[j++] = _XgStrDup(ptr[i]);
        }
    }
    if (ptr)
        _XgFreeStringArray(ptr, items);
    return result;
}

char          **
#ifdef _NO_PROTO
_XgStringArrayAdd(ptr, items, newitem)
    char          **ptr;
    int             items;
    char           *newitem;
#else
_XgStringArrayAdd(char **ptr, int items, char *newitem)
#endif
{
    int             i = 0;
    char          **result;
    result = (char **) _XgCalloc(items + 1, sizeof(char *));

    if (ptr && items) {
        for (i = 0; i < items; i++) {
            result[i] = _XgStrDup(ptr[i]);
        }
    }
    result[i] = _XgStrDup(newitem);
    if (ptr)
        _XgFreeStringArray(ptr, items);
    return result;
}

char          **
#ifdef _NO_PROTO
_XgCopyStringArray(ptr, items)
    char          **ptr;
    int             items;
#else
_XgCopyStringArray(char **ptr, int items)
#endif
{
    int             i;
    char          **result;
    result = (char **) _XgCalloc(items, sizeof(char *));

    for (i = 0; i < items; i++) {
        result[i] = _XgStrDup(ptr[i]);
    }
    return result;
}

#ifdef _NO_PROTO
void
_XgFreeStringArray(ptr, items)
    char          **ptr;
    int             items;
#else
void
_XgFreeStringArray(char **ptr, int items)
#endif
{
    int             i;

    if (ptr) {
        for (i = 0; i < items; i++)
            _XgFree(ptr[i]);
        _XgFree((void *) ptr);
    }
}

#ifdef _NO_PROTO
int
_XgStringArraySearch(ptr, items, str)
    char          **ptr;
    int             items;
    char           *str;
#else
int
_XgStringArraySearch(char **ptr, int items, char *str)
#endif
{
    int             i;
    int             result = 0;

    if (!ptr)
        return -1;
    for (i = 0; i < items; i++)
        if (!strcmp(str, ptr[i]))
            return i;
    return -1;
}

#ifdef _NO_PROTO
int
_XgStringArrayTotalLength(ptr, items)
    char          **ptr;
    int             items;
#else
int
_XgStringArrayTotalLength(char **ptr, int items)
#endif
{
    int             i;
    int             result = 0;

    for (i = 0; i < items; i++)
        result += strlen(ptr[i]);
    return result;
}

char           *
_XgStringArray2CSL(array, count, delim)
    char          **array;
    int             count;
    char           *delim;
{
    long            len;
    char           *result;
    int             i;
    len = _XgStringArrayTotalLength(array, count) + count + 1;
    result = _XgMalloc(len);
    *result = 0;
    for (i = 0; i < count; i++) {
        strcat(result, array[i]);
        if (i != (count - 1))
            strcat(result, delim);
    }
    return result;
}

char          **
_XgCSL2StringArray(s, cp)
    char           *s;
    int            *cp;
{
    char          **result;
    result = _XgTokenize(s, ",");
    *cp = _XgNumberOfTokens(result);
    return result;
}

XmStringTable
_XgStringArray2XmStringTable(array, nitems)
    char          **array;
    int             nitems;
{
    XmStringTable   ret = (XmStringTable) _XgCalloc(nitems + 1, sizeof(XmString));
    int             i;

    for (i = 0; i < nitems; i++) {
        ret[i] = XmStringCreateSimple(array[i]);
    }
    ret[i] = NULL;
    return ret;
}

char **
_XgXmStringTable2StringArray(table, nitems)
    XmStringTable table;
    int nitems;
{
    char ** ret = (char **) _XgCalloc(nitems + 1, sizeof(char *));
    int i;

    for ( i = 0; i < nitems; i++ ) {
        XmStringGetLtoR(table[i], XmSTRING_DEFAULT_CHARSET, &ret[i]);
    }
    ret[i] = NULL;
    return ret;
}

#ifdef _NO_PROTO
void
_XgPutStringArrayInList(list, items, count)
    Widget          list;
    char          **items;
    int             count;
#else
void
_XgPutStringArrayInList(Widget list, char **items, int count)
#endif
{
    XmStringTable   str_list;
    int             i;

    XmListDeselectAllItems(list);

    str_list = (XmStringTable) XtMalloc(count * sizeof(XmString *));

    for (i = 0; i < count; i++)
        str_list[i] = XmStringCreateLtoR(items[i], XmSTRING_DEFAULT_CHARSET);

    XtVaSetValues(list,
                  XmNitems, str_list,
                  XmNitemCount, count,
                  NULL);

    for (i = 0; i < count; i++)
        XmStringFree(str_list[i]);
    XtFree(str_list);
}

char           *
#ifdef _NO_PROTO
_XgStripNewLine(s)
    char           *s;
#else
_XgStripNewLine(char *s)
#endif
{
    char           *ptr = s;

    while (*ptr) {
        if (*ptr == '\n')
            *ptr = '\0';
        ptr++;
    }
    return s;
}

char          **
#ifdef _NO_PROTO
_XgPutFileInStringArray(theFile, countPtr)
    FILE           *theFile;
    int            *countPtr;
#else
_XgPutFileInStringArray(FILE * theFile, int *countPtr)
#endif
{
    char            s[256];
    char           *stk[256];
    int             sp = 0;
    char           *stat;
    XmStringTable   str_list;
    char          **result;
    int             count;
    int             i;

    do {
        stat = fgets(s, 255, theFile);
        _XgStripNewLine(s);

        if (stat) {
            stk[sp++] = _XgStrDup(s);
        }
    } while (stat);

    count = sp;
    result = (char **) _XgCalloc(count, sizeof(char *));

    for (i = 0; i < count; i++) {
        result[i] = stk[i];
    }
    *countPtr = count;
    return result;
}
