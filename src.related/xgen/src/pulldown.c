/**********************************************************************
   pulldown.c   - create pulldown objects
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

/***************************************************************
 * CreatePulldown - creates a pulldown in menu bar menuBar.
 **************************************************************/
Widget
CreatePulldown(object, menuBar)
    InterfaceObject                *object;
    Widget                          menuBar;
{
    InterfaceObject                *o = object->objects;
    int                             n;
    Resource                       *resource;
    Widget                          pulldownW, cb;
    Widget                          retWidget = NULL;

    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(object, &n);
    pulldownW = XmCreatePulldownMenu(menuBar, "pulldown", args, n);
    /* XtManageChild(pulldownW); */
    SetObjectFont(object, &n);
    XtSetArg(args[n], XmNsubMenuId, pulldownW);
    n++;
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "titlestring"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        cb = XmCreateCascadeButton(menuBar, resource->val.cval, args, n);
    } else
        cb = XmCreateCascadeButton(menuBar, object->name, args, n);
    XtManageChild(cb);

    while (o) {
        switch (o->type) {
        case SEPARATOR:
            retWidget = o->widget = CreateSeparator(o, pulldownW, NULL);
            break;
        case PUSHBUTTON:
            retWidget = o->widget = CreateButton(o, pulldownW, NULL, NULL);
            break;
        }
        o = o->next;
    }
    return retWidget;
}
