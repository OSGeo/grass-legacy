/**********************************************************************
   message.c    - create a message object
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
#include "xgen.h"

Widget
CreateMessage(object, widget, parentPaned)
    InterfaceObject                *object;
    Widget                          widget;
    Boolean                         parentPaned;
{
    Resource                       *resource;
    static int                      nummessages = 0;
    char                            messagename[80];
    XmFontList                      fontList;
    XmString                        xmmessage;
    int                             columns;
    int                             n;
    Widget                          row_col = NULL;
    Widget                          messageW = NULL;
    Boolean                         usedTitle;
    MessageInfo                    *mi, *miHead;

    /* check for columns specd */
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "columns"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        columns = resource->val.ival;
    } else
        columns = 20;

    /* check for font specd */
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "font"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        fontList = XmFontListCreate(
                   XLoadQueryFont(xgenGD.display, resource->val.cval), SDC);
    } else
        fontList = NULL;

    usedTitle = False;
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "titlestring"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        miHead = mi = ProcessMessage(resource->val.cval, columns);
        usedTitle = True;
    } else
        miHead = mi = ProcessMessage(object->name, columns);

    if (!parentPaned) {
        n = 0;
        SetGlobalArgs(&n, NOFONTS);
        SetObjectGeometryArgs(object, &n);
        SetObjectColorArgs(object, &n);
        XtSetArg(args[n], XmNpacking, XmPACK_COLUMN);
        n++;
        XtSetArg(args[n], XmNisAligned, False);
        n++;
        XtSetArg(args[n], XmNnumColumns, 1);
        n++;
        row_col = XmCreateRowColumn(widget, "rc", args, n);
        XtManageChild(row_col);
    }
    n = 0;
    SetGlobalArgs(&n, NOFONTS);
    if (fontList != NULL) {
        XtSetArg(args[n], XmNfontList, fontList);
        n++;
    }
    /* KAB - add other resources in here... */
    if (parentPaned)
        SetObjectGeometryArgs(object, &n);
    SetObjectColorArgs(object, &n);
    SetObjectAlignmentArgs(object, &n);
    XtSetArg(args[n], XmNlabelType, XmSTRING);
    n++;
    while (mi) {
        char                           *ptr;

        nummessages++;
        sprintf(messagename, "message%03d", nummessages);

        if (usedTitle)
            ptr = strpart(resource->val.cval, mi->startpos, mi->endpos);
        else
            ptr = strpart(object->name, mi->startpos, mi->endpos);
        xmmessage = XmStringLtoRCreate(ptr, SDC);
        XtSetArg(args[n], XmNlabelString, xmmessage);
        if (!parentPaned)
            messageW = XmCreateLabel(row_col, messagename, args, n + 1);
        else
            messageW = XmCreateLabel(widget, messagename, args, n + 1);
        XtManageChild(messageW);
        mi = mi->next;
    }
    FreeMIList(miHead);
    return messageW;
}
