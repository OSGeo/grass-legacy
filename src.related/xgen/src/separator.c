/**********************************************************************
   separator.c  - create separator object
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
CreateSeparator(object, widget, numChildren)
    InterfaceObject                *object;
    Widget                          widget;
    int                            *numChildren;
{
    static int                      numseparators = 0;
    char                            separatorname[80];
    Resource                       *resource;
    int                             n;
    Widget                          separatorW;

    numseparators++;
    sprintf(separatorname, "separator%03d", numseparators);

    n = 0;
    SetGlobalArgs(&n, NOFONTS);
    /* KAB - add other resources in here... */
    SetObjectGeometryArgs(object, &n);
    SetObjectColorArgs(object, &n);
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "orientation"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (!strcmp(resource->val.cval, "horizontal")) {
            XtSetArg(args[n], XmNorientation, XmHORIZONTAL);
            n++;
        } else if (!strcmp(resource->val.cval, "vertical")) {
            XtSetArg(args[n], XmNorientation, XmVERTICAL);
            n++;
        } else {

            sprintf(errorbuf, "illegal separator orientation in object [%s]",
                    object->name);
            XgenWarning("create separator", errorbuf);
        }
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "separatortype"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (!strcmp(resource->val.cval, "singleline")) {
            XtSetArg(args[n], XmNseparatorType, XmSINGLE_LINE);
            n++;
        } else if (!strcmp(resource->val.cval, "doubleline")) {
            XtSetArg(args[n], XmNseparatorType, XmDOUBLE_LINE);
            n++;
        } else if (!strcmp(resource->val.cval, "singledashedline")) {
            XtSetArg(args[n], XmNseparatorType, XmSINGLE_DASHED_LINE);
            n++;
        } else if (!strcmp(resource->val.cval, "doubledashedline")) {
            XtSetArg(args[n], XmNseparatorType, XmDOUBLE_DASHED_LINE);
            n++;
        } else if (!strcmp(resource->val.cval, "noline")) {
            XtSetArg(args[n], XmNseparatorType, XmNO_LINE);
            n++;
        } else if (!strcmp(resource->val.cval, "shadowetchedin")) {
            XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN);
            n++;
        } else if (!strcmp(resource->val.cval, "shadowetchedout")) {
            XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_OUT);
            n++;
        } else {

            sprintf(errorbuf, "illegal separatortype in object [%s]",
                    object->name);
            XgenWarning("create separator", errorbuf);
            XtSetArg(args[n], XmNseparatorType, XmSINGLE_LINE);
            n++;
        }
    }
    separatorW = XmCreateSeparator(widget, separatorname, args, n);
    if (numChildren != NULL)
        *numChildren += 1;
    XtManageChild(separatorW);
    return separatorW;
}
