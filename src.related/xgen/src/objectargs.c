/**********************************************************************
   objectargs.c - process common Arg's for object creation
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

void
SetObjectGeometryArgs(object, n)
    InterfaceObject                *object;
    int                            *n;
{
    Resource                       *resource;
    int                             dpyWidth = DisplayWidth(xgenGD.display, xgenGD.screen), dpyHeight = DisplayHeight(xgenGD.display, xgenGD.screen);

    if (NULL != (resource = IndexResource((char *) object, OBJECT, "x"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (abs(resource->val.ival) > dpyWidth) {
            sprintf(errorbuf, "x value in object [%s] out of range, ",
                    object->name);
            sprintf(errorbuf, "%sthe screen is only %d pixels wide",
                    errorbuf, dpyWidth);
            XgenWarning("set object geometry", errorbuf);
            resource->val.ival = 0;
        }
        XtSetArg(args[*n], XmNx, resource->val.ival);
        (*n)++;
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "y"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (abs(resource->val.ival) > dpyHeight) {
            sprintf(errorbuf, "y value in object [%s] out of range, ",
                    object->name);
            sprintf(errorbuf, "%sthe screen is only %d pixels tall",
                    errorbuf, dpyHeight);
            XgenWarning("set object geometry", errorbuf);
            resource->val.ival = 0;
        }
        XtSetArg(args[*n], XmNy, resource->val.ival);
        (*n)++;
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "width"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        XtSetArg(args[*n], XmNwidth, resource->val.ival);
        (*n)++;
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "height"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        XtSetArg(args[*n], XmNheight, resource->val.ival);
        (*n)++;
    }
}


void
SetObjectColorArgs(object, n)
    InterfaceObject                *object;
    int                            *n;
{
    Resource                       *resource;
    XColor                          color;

    if (object != NULL &&
            NULL != (resource = IndexResource((char *) object, OBJECT, "foreground"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (XParseColor(xgenGD.display, xgenGD.cmap,
                        resource->val.cval, &color) != 0) {
            if (XAllocColor(xgenGD.display, xgenGD.cmap, &color) == 0) {

                sprintf(errorbuf, "couldn't allocate color %s in object %s",
                        resource->val.cval, object->name);
                XgenWarning("set object color", errorbuf);
            } else {
                XtSetArg(args[*n], XmNforeground, color.pixel);
                (*n)++;
            }
        } else {

            sprintf(errorbuf, "invalid foreground color %s in object %s",
                    resource->val.cval, object->name);
            XgenWarning("set object color", errorbuf);
        }
    } else if (xgenGD.g_fg != NULL) {
        XtSetArg(args[*n], XmNforeground, xgenGD.g_fgs.pixel);
        (*n)++;
    }
    if (object != NULL &&
            NULL != (resource = IndexResource((char *) object, OBJECT, "background"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (XParseColor(xgenGD.display, xgenGD.cmap,
                        resource->val.cval, &color) != 0) {
            if (XAllocColor(xgenGD.display, xgenGD.cmap, &color) == 0) {

                sprintf(errorbuf, "couldn't allocate color %s in object %s",
                        resource->val.cval, object->name);
                XgenWarning("set object color", errorbuf);
            } else {
                XtSetArg(args[*n], XmNbackground, color.pixel);
                (*n)++;
            }
        } else {

            sprintf(errorbuf, "invalid background color %s in object %s",
                    resource->val.cval, object->name);
            XgenWarning("set object color", errorbuf);
        }
    } else if (xgenGD.g_bg) {
        XtSetArg(args[*n], XmNbackground, xgenGD.g_bgs.pixel);
        (*n)++;
    }
}


void
SetTableObjectColorArgs(object, n)
/* ARGSUSED */
    InterfaceObject                *object;
    int                            *n;
{
    if (xgenGD.g_fg) {
    }
}

void
SetObjectReverseColorArgs(object, n)
    InterfaceObject                *object;
    int                            *n;
{
    Resource                       *resource;
    XColor                          color;

    if (object != NULL &&
            NULL != (resource = IndexResource((char *) object, OBJECT, "foreground"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (XParseColor(xgenGD.display, xgenGD.cmap,
                        resource->val.cval, &color) != 0) {
            if (XAllocColor(xgenGD.display, xgenGD.cmap, &color) == 0) {

                sprintf(errorbuf, "couldn't allocate color %s in object %s",
                        resource->val.cval, object->name);
                XgenWarning("set object color", errorbuf);
            } else {
                XtSetArg(args[*n], XmNbackground, color.pixel);
                (*n)++;
            }
        } else {

            sprintf(errorbuf, "invalid foreground color %s in object %s",
                    resource->val.cval, object->name);
            XgenWarning("set object color", errorbuf);
        }
    } else if (xgenGD.g_fg) {
        XtSetArg(args[*n], XmNbackground, xgenGD.g_fgs.pixel);
        (*n)++;
    }
    if (object != NULL &&
            NULL != (resource = IndexResource((char *) object, OBJECT, "background"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (XParseColor(xgenGD.display, xgenGD.cmap,
                        resource->val.cval, &color) != 0) {
            if (XAllocColor(xgenGD.display, xgenGD.cmap, &color) == 0) {

                sprintf(errorbuf, "couldn't allocate color %s in object %s",
                        resource->val.cval, object->name);
                XgenWarning("set object color", errorbuf);
            } else {
                XtSetArg(args[*n], XmNforeground, color.pixel);
                (*n)++;
            }
        } else {

            sprintf(errorbuf, "invalid background color %s in object %s",
                    resource->val.cval, object->name);
            XgenWarning("set object color", errorbuf);
        }
    } else if (xgenGD.g_bg) {
        XtSetArg(args[*n], XmNforeground, xgenGD.g_bgs.pixel);
        (*n)++;
    }
}

void
SetObjectFont(object, n)
    InterfaceObject                *object;
    int                            *n;
{
    Resource                       *resource;

    if (NULL != (resource = IndexResource((char *) object, OBJECT, "font"))) {
        XFontStruct                    *fs = XLoadQueryFont(xgenGD.display, resource->val.cval);
        if (fs) {
            XtSetArg(args[*n], XmNfontList, XmFontListCreate(fs, SDC));
            (*n)++;
        } else {
            sprintf(errorbuf, "couldn't load font: %s", resource->val.cval);
            XgenWarning("set object font", errorbuf);
        }
    }
}

void
SetObjectAlignmentArgs(object, n)
    InterfaceObject                *object;
    int                            *n;
{
    Resource                       *resource;

    if (NULL != (resource = IndexResource((char *) object, OBJECT, "alignment"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (!strcmp(resource->val.cval, "left")) {
            XtSetArg(args[*n], XmNalignment, XmALIGNMENT_BEGINNING);
            (*n)++;
        } else if (!strcmp(resource->val.cval, "right")) {
            XtSetArg(args[*n], XmNalignment, XmALIGNMENT_END);
            (*n)++;
        } else if (!strcmp(resource->val.cval, "center")) {
            XtSetArg(args[*n], XmNalignment, XmALIGNMENT_CENTER);
            (*n)++;
        } else {

            sprintf(errorbuf, "invalid alignment in label \"%s\"", object->name);
            XgenWarning("create label object", errorbuf);
        }
    } else {
        XtSetArg(args[*n], XmNalignment, XmALIGNMENT_BEGINNING);
        (*n)++;
    }
}

void
SetGlobalArgs(n, fonts)
    int                            *n;
    int                             fonts;
{
    if (fonts && xgenGD.g_font) {
        XtSetArg(args[*n], XmNfontList, XmFontListCreate(xgenGD.g_fs, SDC));
        (*n)++;
    }
    if (xgenGD.g_bgpix) {
        XtSetArg(args[*n], XmNbackgroundPixmap, xgenGD.g_bgpm);
        (*n)++;
    }
}


void
SetShellColorArgs(shell, n)
    Shell                          *shell;
    int                            *n;
{
    Resource                       *resource;
    XColor                          color;

    if (shell != NULL &&
            NULL != (resource = IndexResource((char *) shell, SHELL, "foreground"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) shell, SHELL);
        if (XParseColor(xgenGD.display, xgenGD.cmap,
                        resource->val.cval, &color) != 0) {
            if (XAllocColor(xgenGD.display, xgenGD.cmap, &color) == 0) {

                sprintf(errorbuf, "couldn't allocate color %s in shell %s",
                        resource->val.cval, shell->name);
                XgenWarning("set shell color", errorbuf);
            } else {
                XtSetArg(args[*n], XmNforeground, color.pixel);
                (*n)++;
            }
        } else {

            sprintf(errorbuf, "invalid foreground color %s in shell %s",
                    resource->val.cval, shell->name);
            XgenWarning("set shell color", errorbuf);
        }
    } else if (xgenGD.g_fg) {
        XtSetArg(args[*n], XmNforeground, xgenGD.g_fgs.pixel);
        (*n)++;
    }
    if (shell != NULL &&
            NULL != (resource = IndexResource((char *) shell, SHELL, "background"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) shell, SHELL);
        if (XParseColor(xgenGD.display, xgenGD.cmap,
                        resource->val.cval, &color) != 0) {
            if (XAllocColor(xgenGD.display, xgenGD.cmap, &color) == 0) {

                sprintf(errorbuf, "couldn't allocate color %s in shell %s",
                        resource->val.cval, shell->name);
                XgenWarning("set shell color", errorbuf);
            } else {
                XtSetArg(args[*n], XmNbackground, color.pixel);
                (*n)++;
            }
        } else {

            sprintf(errorbuf, "invalid background color %s in shell %s",
                    resource->val.cval, shell->name);
            XgenWarning("set shell color", errorbuf);
        }
    } else if (xgenGD.g_bg) {
        XtSetArg(args[*n], XmNbackground, xgenGD.g_bgs.pixel);
        (*n)++;
    }
}
