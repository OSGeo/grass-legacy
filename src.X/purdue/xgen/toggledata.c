#include "xgen.h"
void DeleteToggleInfo();
void DeleteToggleData();

void
AddToggleInfo(name,td)
    char *name;
	ToggleData *td;
{
	ToggleInfo *tmp = xgenGD.toggleInfo;

	if ( tmp == NULL ) {
		xgenGD.toggleInfo = (ToggleInfo *)XtMalloc(sizeof(ToggleInfo));
		xgenGD.toggleInfo->objectName = name;
		xgenGD.toggleInfo->toggleData = td;
		xgenGD.toggleInfo->next = NULL;
		return;
	}
	for (;tmp->next != NULL;tmp = tmp->next);
	tmp->next = (ToggleInfo *)XtMalloc(sizeof(ToggleInfo));
	tmp = tmp->next;
	tmp->objectName = name;
	tmp->toggleData = td;
	tmp->next = NULL;
}

void DeleteToggleInfo(name)
	char *name;
{
	ToggleInfo *goner;
	ToggleInfo *tmp = xgenGD.toggleInfo;

	while(tmp) {
		if (!strcmp(tmp->objectName,name)) {
			goner = tmp;
			break;
		}
		tmp = tmp->next;
	}

	if ( goner == xgenGD.toggleInfo )
		xgenGD.toggleInfo = goner->next;
	else {
		for ( tmp = xgenGD.toggleInfo; (tmp != NULL) && (tmp->next != goner);
			  tmp = tmp->next) ;
		if ( tmp == NULL ) return;
		tmp->next = tmp->next->next;
	}

	DeleteToggleData(goner->toggleData);
	XtFree(goner);
}

void DeleteToggleData(td)
	ToggleData *td;
{
	if ( td->next ) DeleteToggleData(td->next);
	else {
		XtFree(td->name);
		XtFree(td);
	}
	return;
}

ToggleData *
IndexToggleData(name)
    char *name;
{
	ToggleInfo *tmp = xgenGD.toggleInfo;

	while(tmp) {
		if (!strcmp(tmp->objectName,name)) return(tmp->toggleData);
		tmp = tmp->next;
	}
	return NULL;
}

Boolean
IsToggleSet(name)
    char *name;
{
	ToggleInfo *tmp = xgenGD.toggleInfo;

	while(tmp) {
		ToggleData *td = tmp->toggleData;

		while(td) {
		    if (!strcmp(td->name,name) && td->set) return True;
			td = td->next;
		}
		tmp = tmp->next;
	}
	return False;
}
