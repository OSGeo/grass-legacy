/**********************************************************************
   toggledata.c - operate on toggle data structure
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
void                            DeleteToggleInfo();
void                            DeleteToggleData();

void
AddToggleInfo(name, td)
    char                           *name;
    ToggleData                     *td;
{
    ToggleInfo                     *tmp = xgenGD.toggleInfo;

    if (tmp == NULL) {
        xgenGD.toggleInfo = (ToggleInfo *) XtMalloc(sizeof(ToggleInfo));
        xgenGD.toggleInfo->objectName = name;
        xgenGD.toggleInfo->toggleData = td;
        xgenGD.toggleInfo->next = NULL;
        return;
    }
    while (tmp->next != NULL)
        tmp = tmp->next;
    tmp->next = (ToggleInfo *) XtMalloc(sizeof(ToggleInfo));
    tmp = tmp->next;
    tmp->objectName = name;
    tmp->toggleData = td;
    tmp->next = NULL;
}

void 
DeleteToggleInfo(name)
    char                           *name;
{
    ToggleInfo                     *goner = NULL;
    ToggleInfo                     *tmp = xgenGD.toggleInfo;

    while (tmp) {
        if (!strcmp(tmp->objectName, name)) {
            goner = tmp;
            break;
        }
        tmp = tmp->next;
    }

    if (goner == xgenGD.toggleInfo)
        xgenGD.toggleInfo = goner->next;
    else {
        tmp = xgenGD.toggleInfo;
        while ((tmp != NULL) && (tmp->next != goner))
            tmp = tmp->next;
        if (tmp == NULL)
            return;
        tmp->next = tmp->next->next;
    }

    DeleteToggleData(goner->toggleData);
    XtFree((char *) goner);
}

void 
DeleteToggleData(td)
    ToggleData                     *td;
{
    if (td->next)
        DeleteToggleData(td->next);
    else {
        XtFree(td->name);
        XtFree((char *) td);
    }
    return;
}

ToggleData                     *
IndexToggleData(name)
    char                           *name;
{
    ToggleInfo                     *tmp = xgenGD.toggleInfo;

    while (tmp) {
        if (!strcmp(tmp->objectName, name))
            return (tmp->toggleData);
        tmp = tmp->next;
    }
    return NULL;
}

Boolean
IsToggleSet(o,name)
    InterfaceObject                *o;
    char                           *name;
{
    ToggleInfo                     *tmp = xgenGD.toggleInfo;

    while (tmp) {
        ToggleData                     *td = tmp->toggleData;

        while (td) {
            if (!strcmp(td->name, name) && o->widget == XtParent(td->widget) 
		&& td->set)
                return True;
            td = td->next;
        }
        tmp = tmp->next;
    }
    return False;
}
