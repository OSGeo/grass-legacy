#include "xgen.h"

char *GetObjectValue(o,tinfo)
	InterfaceObject *o;
	char *tinfo;
{
	XmString xmstring;
	Resource *resource;
	double ten();
	char *text;
	int ival;
	int n;

	switch(o->type) {
		case LABEL:
			n = 0;
			XtSetArg(args[n],XmNlabelString,&xmstring); n++;
			XtGetValues(o->widget,args,n);
			if ( !XmStringEmpty(xmstring) )
				XmStringGetLtoR(xmstring,SDC,&text);
			break;
		case LIST:
			{
			char value[1024];
			ToggleData *tdp;

			value[0] = NULL;
			if ( NULL != (resource = IndexResource(o,OBJECT,"listelement"))){
				while ( resource ) {
					if ( !strcmp(resource->name,"listelement") &&
						 IsListSelected(resource->val.cval)) {
						if ( resource->next && 
							 !strcmp(resource->next->name,"valuestring")) {
							if ( value[0] == NULL )
							    sprintf(value,"%s",resource->next->val.cval);
							else
							    sprintf(value,"%s%s",value,resource->next->val.cval);
						} else {
							if ( value[0] == NULL )
							    sprintf(value,"%s",resource->val.cval);
							else
							    sprintf(value,"%s%s",value,resource->val.cval);
						}
					}
					resource = resource->next;
				}
			} 
			text = XtMalloc(strlen(value) + 1);
			strcpy(text,value);
			}
			break;
			
			break;
		case PUSHBUTTON:
			n = 0;
			XtSetArg(args[n],XmNlabelString,&xmstring); n++;
			XtGetValues(o->widget,args,n);
			if ( !XmStringEmpty(xmstring) )
				XmStringGetLtoR(xmstring,SDC,&text);
			break;
		case TEXTENTRY:
			n = 0;
			XtSetArg(args[n],XmNvalue,&text); n++;
			XtGetValues(o->widget,args,n);
			break;
		case FILESELECT:
		/* KAB NYI */
			break;
		case SLIDER:
			n = 0;
			XmScaleGetValue(o->widget,&ival);
			text = (char *)XtMalloc(24);
			if ( NULL != (resource = IndexResource(o,OBJECT,"decimalpoints"))) {
				sprintf(text,"%f",
					(double)ival/ten(resource->val.ival));
			} else {
				sprintf(text,"%d",ival);
			}
			break;
		case TOGGLE:
			{
			char value[1024];
			ToggleData *tdp;

			value[0] = NULL;
			if ( NULL != (resource = IndexResource(o,OBJECT,"listelement"))){
				while ( resource ) {
					if ( !strcmp(resource->name,"listelement") &&
						 IsToggleSet(resource->val.cval)) {
						if ( resource->next && 
							 !strcmp(resource->next->name,"valuestring")) {
							if ( value[0] == NULL )
							    sprintf(value,"%s",resource->next->val.cval);
							else
							    sprintf(value,"%s%s",value,resource->next->val.cval);
						} else {
							if ( value[0] == NULL )
							    sprintf(value,"%s",resource->val.cval);
							else
							    sprintf(value,"%s%s",value,resource->val.cval);
						}
					}
					resource = resource->next;
				}
			} 
			text = XtMalloc(strlen(value) + 1);
			strcpy(text,value);
			}
			break;
		case TABLE:
			{
			int rows;
			int columns;

			if ( NULL != (resource = IndexResource(o,OBJECT,"rows"))) {
		        if ( resource->variable ) ExpandVariable(resource);
				rows = resource->val.ival;
			} else {
				int n = 0;
				Arg arg;

				XtSetArg(arg,XmNrows,&rows);
				XtGetValues(o->widget,arg,n);
			}
			if ( NULL != (resource = IndexResource(o,OBJECT,"columns"))) {
		        if ( resource->variable ) ExpandVariable(resource);
				columns = resource->val.ival;
			} else {
				int n = 0;
				Arg arg;

				XtSetArg(arg,XmNcolumns,&columns);
				XtGetValues(o->widget,arg,n);
			}
			if ( tinfo == NULL ) {
				int r,
					bytes = 0;

				for ( r = 0; r < rows; r++ ) {
					bytes += strlen(XmTableGetRow(o->widget,r+1)) + 1;
				}
				text = XtMalloc(++bytes);
				for ( r = 0; r < rows; r++ ) {
					if ( r == 0 ) {
					    strcpy(text,XmTableGetRow(o->widget,r+1));
					    strcat(text," ");
					} else {
					    strcat(text,XmTableGetRow(o->widget,r+1));
					    strcat(text," ");
					}
				}
			} else if ( NULL != rindex(tinfo,',') ) {
				int r = atoi(strtok(tinfo,",")),
					c = atoi(strtok(NULL,"")),
					bytes = strlen(XmTableGetValue(o->widget,r,c));

					text = XtMalloc(++bytes);
					strcpy(text,XmTableGetValue(o->widget,r,c));
			} else {
				int r = atoi(tinfo),
					bytes = strlen(XmTableGetRow(o->widget,r));

					text = XtMalloc(++bytes);
					strcpy(text,XmTableGetRow(o->widget,r));
			}
		}
			break;
		case MESSAGE:
			break;
	}
	return text;
}

double 
ten(n)
	int n;
{
	if ( n == 1 ) return(10.0);
	return(10.0*ten(n - 1));
}
