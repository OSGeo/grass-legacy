/**********************************************************************
   slider.c     - create slider object
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
CreateSlider(object, widget)
    InterfaceObject                *object;
    Widget                          widget;
{
    static int                      numsliders = 0;
    Resource                       *resource;
    XmString                        xmstring;
    char                            slidername[80];
    Widget                          sliderW;
    int                             n;

    numsliders++;
    sprintf(slidername, "slider%03d", numsliders);

    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectGeometryArgs(object, &n);
    SetObjectColorArgs(object, &n);
    SetObjectFont(object, &n);
    XtSetArg(args[n], XmNshowValue, True);
    n++;
    XtSetArg(args[n], XmNhighlightOnEnter, True);
    n++;
    XtSetArg(args[n], XmNhighlightThickness, 2);
    n++;
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "maximum"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        XtSetArg(args[n], XmNmaximum, resource->val.ival);
        n++;
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "minimum"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        XtSetArg(args[n], XmNminimum, resource->val.ival);
        n++;
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "startvalue"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        XtSetArg(args[n], XmNvalue, resource->val.ival);
        n++;
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "sliderwidth"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        XtSetArg(args[n], XmNscaleWidth, resource->val.ival);
        n++;
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "sliderheight"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        XtSetArg(args[n], XmNscaleHeight, resource->val.ival);
        n++;
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "orientation"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (!strcmp(resource->val.cval, "vertical")) {
            XtSetArg(args[n], XmNorientation, XmVERTICAL);
            n++;
            XtSetArg(args[n], XmNprocessingDirection, XmMAX_ON_TOP);
            n++;
        } else if (!strcmp(resource->val.cval, "horizontal")) {
            XtSetArg(args[n], XmNorientation, XmHORIZONTAL);
            n++;
            XtSetArg(args[n], XmNprocessingDirection, XmMAX_ON_RIGHT);
            n++;
        } else {

            sprintf(errorbuf, "invalid orientation in slider \"%s\"\n",
                    object->name);
            XgenWarning("create slider", errorbuf);
        }
    } else {
        XtSetArg(args[n], XmNorientation, XmHORIZONTAL);
        n++;
        XtSetArg(args[n], XmNprocessingDirection, XmMAX_ON_RIGHT);
        n++;
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "titlestring"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        xmstring = XmStringCreateLtoR(resource->val.cval, SDC);
    } else
        xmstring = XmStringCreateLtoR(object->name, SDC);
    XtSetArg(args[n], XmNtitleString, xmstring);
    n++;
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "decimalpoints"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (resource->val.ival > 0) {
            XtSetArg(args[n], XmNdecimalPoints, resource->val.ival);
            n++;
        }
    }
    sliderW = XmCreateScale(widget, slidername, args, n);
    XtManageChild(sliderW);
    return sliderW;
}
