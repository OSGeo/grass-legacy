
#include "xgen.h"

void
ClearTable(o)
	InterfaceObject *o;
{
	Resource *resource;
	int rows, columns, r, c;

	if ( NULL != (resource = IndexResource(o,OBJECT,"rows"))) {
		if ( resource->variable ) ExpandVariable(resource);
		rows = resource->val.ival;
	} else
		XgenFatalError("clear table","no row count");
	if ( NULL != (resource = IndexResource(o,OBJECT,"columns"))) {
		if ( resource->variable ) ExpandVariable(resource);
		columns = resource->val.ival;
	} else
		XgenFatalError("clear table","no column count");

	for ( r = 1; r <= rows; r ++ )
		for ( c = 1; c <= columns; c++ )
			XmTableSetValue(o->widget,r,c,"");
}

void
ClearTablePart(o,spec)
	InterfaceObject *o;
	char *spec;
{
	/* index past '(' */
	spec++;
	if ( NULL != rindex(spec,',')) {
		int r = atoi(strtok(spec,",")),
			c = atoi(strtok(NULL,")"));
		
		XmTableSetValue(o->widget,r,c,"");
	} else {
		int r = atoi(strtok(spec,")"));
	    Resource *resource;
	    int columns, i;
    
	    if ( NULL != (resource = IndexResource(o,OBJECT,"columns"))) {
		    if ( resource->variable ) ExpandVariable(resource);
		    columns = resource->val.ival;
	    } else
		    XgenFatalError("clear table","no column count");

		for ( i = 0; i < columns; i++ ) {
			XmTableSetValue(o->widget,r,i+1,"");
		}
	}

}

void
ClearToggle(o)
	InterfaceObject *o;
{
	ToggleData *tdp;

	fprintf(stderr,"Toggle clear\n");
	if ( NULL == (tdp = IndexToggleData(o->name))) {
		XgenWarning("clear toggle","couldn't find toggle object");
		return;
	}
	while ( tdp ) {
		if ( tdp->set ) {
			XtSetArg(args[0],XmNset,False);
			XtSetValues(tdp->widget,args,1);
			tdp->set = False;
		}
		tdp = tdp->next;
	}
}


void
ClearList(name)
	char *name;
{
	ListData *ldp;

	fprintf(stderr,"List clear\n");
	if ( NULL == (ldp = IndexListData(name))) {
		XgenWarning("clear list","couldn't find list object");
		return;
	}
	while ( ldp ) {
		if ( ldp->selected ) 
			ldp->selected = False;
		ldp = ldp->next;
	}
}

