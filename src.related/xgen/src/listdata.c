/**********************************************************************
   listdata.c   - operate on list data structure
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
void                            DeleteListInfo();
void                            DeleteListData();

void
AddListInfo(name, ld, type)
    char                           *name;
    ListData                       *ld;
    ListType                        type;
{
    ListInfo                       *tmp = xgenGD.listInfo;

    if (tmp == NULL) {
        xgenGD.listInfo = (ListInfo *) XtMalloc(sizeof(ListInfo));
        xgenGD.listInfo->objectName = name;
        xgenGD.listInfo->listType = type;
        xgenGD.listInfo->listData = ld;
        xgenGD.listInfo->next = NULL;
        return;
    }
    while (tmp->next != NULL)
        tmp = tmp->next;
    tmp->next = (ListInfo *) XtMalloc(sizeof(ListInfo));
    tmp = tmp->next;
    tmp->objectName = name;
    tmp->listType = type;
    tmp->listData = ld;
    tmp->next = NULL;
}

void 
DeleteListInfo(name)
    char                           *name;
{
    ListInfo                       *goner = NULL;
    ListInfo                       *tmp = xgenGD.listInfo;

    while (tmp) {
        if (!strcmp(tmp->objectName, name)) {
            goner = tmp;
            break;
        }
        tmp = tmp->next;
    }

    if (goner == xgenGD.listInfo)
        xgenGD.listInfo = goner->next;
    else {
        tmp = xgenGD.listInfo;
        while ((tmp != NULL) && (tmp->next != goner))
            tmp = tmp->next;
        if (tmp == NULL)
            return;
        tmp->next = tmp->next->next;
    }
    XtFree((char *) goner);
}

void 
DeleteListData(ld)
    ListData                       *ld;
{
    if (ld->next)
        DeleteListData(ld->next);
    else {
        if (ld->valueString)
            XtFree(ld->valueString);
        XmStringFree(ld->item);
        XtFree((char *) ld);
    }
}

ListType
IndexListType(name)
    char                           *name;
{
    ListInfo                       *tmp = xgenGD.listInfo;

    while (tmp) {
        if (!strcmp(tmp->objectName, name))
            return (tmp->listType);
        tmp = tmp->next;
    }
    return (ListType)NULL;
}

ListData                       *
IndexListData(name)
    char                           *name;
{
    ListInfo                       *tmp = xgenGD.listInfo;

    while (tmp) {
        if (!strcmp(tmp->objectName, name))
            return (tmp->listData);
        tmp = tmp->next;
    }
    return ((ListData *) NULL);
}

Boolean
IsListSelected(lname, name)
    char                           *lname;
    char                           *name;
{
    ListData                       *ld = IndexListData(lname);
    XmString                        xmstring;

    xmstring = XmStringCreateLtoR(name, SDC);

    while (ld) {
	if (XmStringCompare(ld->item, xmstring) && ld->selected) {
	    XmStringFree(xmstring);
	    return True;
        }
	ld = ld->next;
    }
    XmStringFree(xmstring);
    return False;
}
