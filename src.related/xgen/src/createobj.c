/**********************************************************************
   createobj.c  - create an object
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

/* returns the last widget created for sizing purposes */
Widget
CreateObject(shell, widget, isDynamic, numChildren, parentPaned)
    Shell                          *shell;
    Widget                          widget;
    Boolean                         isDynamic;
    int                            *numChildren;
    Boolean                         parentPaned;
{
    InterfaceObject                *o = shell->objects;
    Widget                          retwidget = (Widget) 0;
    Resource                       *updatefrom = NULL;

    if (isDynamic) {
        updatefrom = IndexResource((char *) shell, SHELL, "updatefrom");
        if (updatefrom->variable && updatefrom->varValue[0] == '!') {

            updatefrom->val.cval = SaveString(updatefrom->varValue);
            updatefrom->variable = False;
        }
    }
    while (o) {
        switch (o->type) {
        case LABEL:
            o->widget = retwidget = CreateLabel(o, widget, numChildren);
            break;
        case MESSAGE:
            o->widget = retwidget = CreateMessage(o, widget, parentPaned);
            break;
        case LIST:
            o->widget = retwidget = CreateList(o, widget);
            break;
        case PUSHBUTTON:
            o->widget = retwidget =
                CreateButton(o, widget, updatefrom, numChildren, shell);
            break;
        case TEXTENTRY:
            o->widget = retwidget = CreateTextEntry(o, widget);
            break;
        case TABLE:
            o->widget = retwidget = CreateTable(o, widget);
            break;
        case SEPARATOR:
            o->widget = retwidget = CreateSeparator(o, widget, numChildren);
            break;
        case SLIDER:
            o->widget = retwidget = CreateSlider(o, widget);
            break;
        case TOGGLE:
            o->widget = retwidget = CreateToggle(o, widget);
            break;
        case PULLDOWN:
            /* implemented within the shell creation routines */
            break;
        case MULTILINE:
            o->widget = retwidget = CreateMultiLine(o, widget);
            break;
        }
        o = o->next;
    }
    return retwidget;
}
