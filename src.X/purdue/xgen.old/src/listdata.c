#include "xgen.h"

void
AddListInfo(name,ld)
    char *name;
	ListData *ld;
{
	ListInfo *tmp = xgenGD.listInfo;

	if ( tmp == NULL ) {
		xgenGD.listInfo = (ListInfo *)XtMalloc(sizeof(ListInfo));
		xgenGD.listInfo->objectName = name;
		xgenGD.listInfo->listData = ld;
		xgenGD.listInfo->next = NULL;
		return;
	}
	for (;tmp->next != NULL;tmp = tmp->next);
	tmp->next = (ListInfo *)XtMalloc(sizeof(ListInfo));
	tmp = tmp->next;
	tmp->objectName = name;
	tmp->listData = ld;
	tmp->next = NULL;
}

ListData *
IndexListData(name)
    char *name;
{
	ListInfo *tmp = xgenGD.listInfo;

	while(tmp) {
		if (!strcmp(tmp->objectName,name)) return(tmp->listData);
		tmp = tmp->next;
	}
	return NULL;
}

Boolean
IsListSelected(name)
    char *name;
{
	ListInfo *tmp = xgenGD.listInfo;

	while(tmp) {
		ListData *ld = tmp->listData;

		while(ld) {
			XmString xmstring;

			xmstring = XmStringCreateLtoR(name,SDC);
		    if (XmStringCompare(ld->item,xmstring) && ld->selected) 
				return True;
			XmStringFree(xmstring);
			ld = ld->next;
		}
		tmp = tmp->next;
	}
	return False;
}
