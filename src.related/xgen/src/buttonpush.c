/**********************************************************************
   buttonpush.c - perform actions when a button is pushed
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
 *  ButtonPushedCB - the callback associated with all buttons.
 **************************************************************/

void
ButtonPushCB(w, cld, cad)
    Widget                          w;
    caddr_t                         cld;
    caddr_t                         cad;
{
    /***************************************************************
     * the client data field is a pointer to the button's object information
     **************************************************************/
    InterfaceObject                *object = (InterfaceObject *) cld;
    /***************************************************************
     * the call data field is a pointer to the event causing the callback
     * to be called.
     **************************************************************/
    XmAnyCallbackStruct            *cbs = (XmAnyCallbackStruct *) cad;
    Resource                       *resource;
    XmString                        xmLabel;
    char                           *text;
    int                             n;

    /***************************************************************
     * If the event field of the callback struct is NULL we were
     * probably activated by a key press after tabbing to this
     * button, we can't check for a shifted mouse click....
     * If the button press was shifted look for help and display it, else
     * ignore the buttonpress altogther, in either case return.
     **************************************************************/
    if (cbs->event != NULL && cbs->event->xbutton.state & ShiftMask) {
        if (NULL != (resource = IndexResource((char *) object, OBJECT, "help"))) {
            XmString                        help;
            if (resource->variable)
                ExpandVariable(resource, (char *) object, OBJECT);
            help = XmStringCreateLtoR(resource->val.cval, SDC);
            n = 0;
            XtSetArg(args[n], XmNmessageString, help);
            n++;
            XtSetValues(xgenGD.help, args, n);
            XtManageChild(xgenGD.help);
            XmStringFree(help);
        }
        return;
    }
    /* get the buttons label text, and convert it to a character string */
    n = 0;
    XtSetArg(args[n], XmNlabelString, &xmLabel);
    n++;
    XtGetValues(w, args, n);

    XmStringGetLtoR(xmLabel, SDC, &text);

    DoActions(object,text);
}
