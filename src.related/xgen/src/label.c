/**********************************************************************
   label.c      - create a label object
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
CreateLabel(object, widget, numChildren)
    InterfaceObject                *object;
    Widget                          widget;
    int                            *numChildren;
{
    static int                      numlabels = 0;
    char                            labelname[80];
    Resource                       *resource;
    XmString                        xmstring;
    Pixel                           fgpixel, bgpixel;
    XColor                          color;
    int                             n;
    Widget                          labelW;

    numlabels++;
    sprintf(labelname, "label%03d", numlabels);

    n = 0;
    SetGlobalArgs(&n, FONTS);
    /* KAB - add other resources in here... */
    SetObjectGeometryArgs(object, &n);
    SetObjectColorArgs(object, &n);
    SetObjectAlignmentArgs(object, &n);
    SetObjectFont(object, &n);

    if (NULL != (resource = IndexResource((char *) object, OBJECT, "labelpixmap"))) {
        Pixmap                          pixmap;
        char                           *image;
	Resource		       *pixresource = resource;

        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        image = SaveString(resource->val.cval);
        if (NULL != (resource = IndexResource((char *) object, OBJECT, "background"))) {
            if (resource->variable)
                ExpandVariable(resource, (char *) object, OBJECT);
            if (XParseColor(xgenGD.display, xgenGD.cmap,
                            resource->val.cval, &color) != 0) {
                if (XAllocColor(xgenGD.display, xgenGD.cmap, &color) == 0) {

                    sprintf(errorbuf, "couldn't allocate color %s in object %s",
                            resource->val.cval, object->name);
                    XgenWarning("set object color", errorbuf);
                    bgpixel = xgenGD.g_bgs.pixel;
                } else
                    bgpixel = color.pixel;
            } else {

                sprintf(errorbuf, "invalid background color %s in object %s",
                        resource->val.cval, object->name);
                XgenWarning("set object color", errorbuf);
                bgpixel = xgenGD.g_bgs.pixel;
            }
        } else {
            bgpixel = xgenGD.g_bgs.pixel;
        }
        if (NULL != (resource = IndexResource((char *) object, OBJECT, "foreground"))) {
            if (resource->variable)
                ExpandVariable(resource, (char *) object, OBJECT);
            if (XParseColor(xgenGD.display, xgenGD.cmap,
                            resource->val.cval, &color) != 0) {
                if (XAllocColor(xgenGD.display, xgenGD.cmap, &color) == 0) {

                    sprintf(errorbuf, "couldn't allocate color %s in object %s",
                            resource->val.cval, object->name);
                    XgenWarning("set object color", errorbuf);
                    fgpixel = xgenGD.g_fgs.pixel;
                } else
                    fgpixel = color.pixel;
            } else {

                sprintf(errorbuf, "invalid foreground color %s in object %s",
                        resource->val.cval, object->name);
                XgenWarning("set object color", errorbuf);
                fgpixel = xgenGD.g_fgs.pixel;
            }
        } else {
            fgpixel = xgenGD.g_fgs.pixel;
        }
        pixmap = XmGetPixmap(xgenGD.scrptr, image,
                             fgpixel, bgpixel);
        if (pixmap == XmUNSPECIFIED_PIXMAP) {

            sprintf(errorbuf, "labelpixmap [%s] not found", pixresource->val.cval);
            XgenWarning("create label", errorbuf);
        }
        XtSetArg(args[n], XmNlabelType, XmPIXMAP);
        n++;
        XtSetArg(args[n], XmNlabelPixmap, pixmap);
        n++;
    } else {
        if (NULL != (resource = IndexResource((char *) object, OBJECT, "titlestring"))) {
            if (resource->variable)
                ExpandVariable(resource, (char *) object, OBJECT);
            xmstring = XmStringLtoRCreate(resource->val.cval, SDC);
        } else
            xmstring = XmStringLtoRCreate(object->name, SDC);
        XtSetArg(args[n], XmNlabelType, XmSTRING);
        n++;
        XtSetArg(args[n], XmNlabelString, xmstring);
        n++;
    }
    labelW = XmCreateLabel(widget, labelname, args, n);
    if (numChildren != NULL)
        *numChildren += 1;
    XtManageChild(labelW);
    return labelW;
}
