/**********************************************************************
   togglechng.c - perform changes to toggle's state
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
ToggleChangedCB(w, cld, cad)
    Widget                          w;
    caddr_t                         cld;
    caddr_t                         cad;
{
    InterfaceObject                *object = (InterfaceObject *) cld;
    XmToggleButtonCallbackStruct   *cbs = (XmToggleButtonCallbackStruct *) cad;
    int                             n;
    XmString                        xmlabel;
    ToggleData                     *tdp;
    char                           *text;

    n = 0;
    XtSetArg(args[n], XmNlabelString, &xmlabel);
    n++;
    XtGetValues(w, args, n);

    XmStringGetLtoR(xmlabel, SDC, &text);

    if (NULL == (tdp = IndexToggleData(object->name))) {
        XgenFatalError("indexing toggle data", "no such data");
    }
    while (tdp) {
        if (!strcmp(tdp->name, text))
            tdp->set = cbs->set;
        tdp = tdp->next;
    }
    if ( cbs->set == True )
	ToggleCB(w, cld, cad);
}

void
RadioChangedCB(w, cld, cad)
    Widget                          w;
    caddr_t                         cld;
    caddr_t                         cad;
{
    InterfaceObject                *object = (InterfaceObject *) cld;
    XmToggleButtonCallbackStruct   *cbs = (XmToggleButtonCallbackStruct *) cad;
    int                             n;
    XmString                        xmlabel;
    ToggleData                     *tdp;
    char                           *text;

    n = 0;
    XtSetArg(args[n], XmNlabelString, &xmlabel);
    n++;
    XtGetValues(w, args, n);

    XmStringGetLtoR(xmlabel, SDC, &text);

    if (NULL == (tdp = IndexToggleData(object->name))) {
        XgenFatalError("indexing toggle data", "no such data");
    }
    while (tdp) {
        if (!strcmp(tdp->name, text)) {
            if (tdp->set && cbs->set) {
                tdp->radioIgnoreOff = True;
                tdp->set = cbs->set;
            } else if (tdp->set && !(cbs->set) && tdp->radioIgnoreOff) {
                tdp->radioIgnoreOff = False;
            } else
                tdp->set = cbs->set;
        }
        tdp = tdp->next;
    }
    if ( cbs->set == True )
	ToggleCB(w, cld, cad);
}

void
ToggleCB(w, cld, cad)
    Widget                          w;
    caddr_t                         cld;
    caddr_t                         cad;
{
    InterfaceObject                *object = (InterfaceObject *) cld;
    XmString                        xmlabel;
    char                           *text;
    int                             n;

    n = 0;
    XtSetArg(args[n], XmNlabelString, &xmlabel);
    n++;
    XtGetValues(w, args, n);

    XmStringGetLtoR(xmlabel, SDC, &text);

    DoActions(object, text);

}
