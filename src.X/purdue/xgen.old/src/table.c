#include "xgen.h"

Widget
CreateTable(object,widget)
    InterfaceObject *object;
    Widget widget;
{
    static int numtables = 0;
    char tablename[80];
    Resource *resource;
    XmString xmstring;
    int n;
    int numcols, numrows, i;
    char *tok;
    Widget tableW;

    numtables++;
    sprintf(tablename,"table%03d",numtables);

    n = 0;
    SetGlobalArgs(&n,NOFONTS);
    /* KAB - add other resources in here... */
    SetObjectGeometryArgs(object,&n);
    SetObjectColorArgs(object,&n);
    if ( NULL != (resource = IndexResource(object,OBJECT,"font"))) {
        XFontStruct *fnt;
        fnt = XLoadQueryFont(resource->val.cval);
        XtSetArg(args[n],XmNheadingFontList,XmFontListCreate(fnt,SDC)); n++;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"titlestring"))) 
        xmstring = XmStringCreateLtoR(resource->val.cval,SDC);
	else
        xmstring = XmStringCreateLtoR(object->name,SDC);
    XtSetArg(args[n],XmNtitleString,xmstring); n++;
    if ( NULL != (resource = IndexResource(object,OBJECT,"rows"))) {
		if ( resource->variable ) ExpandVariable(resource);
        XtSetArg(args[n],XmNrows,resource->val.ival); n++;
        numrows = resource->val.ival;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"rowsdisplayed"))) {
		if ( resource->variable ) ExpandVariable(resource);
        XtSetArg(args[n],XmNrowsDisplayed,resource->val.ival); n++;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"columns"))) {
		if ( resource->variable ) ExpandVariable(resource);
        XtSetArg(args[n],XmNcolumns,resource->val.ival); n++;
        numcols = resource->val.ival;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"columnsdisplayed"))) {
		if ( resource->variable ) ExpandVariable(resource);
        XtSetArg(args[n],XmNcolumnsDisplayed,resource->val.ival); n++;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"columnwidth"))) {
		if ( resource->variable ) ExpandVariable(resource);
        XtSetArg(args[n],XmNcolumnWidth,resource->val.ival); n++;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"rowheight"))) {
		if ( resource->variable ) ExpandVariable(resource);
        XtSetArg(args[n],XmNrowHeight,resource->val.ival); n++;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"entryfont"))) {
        XFontStruct *fnt;
        fnt = XLoadQueryFont(resource->val.cval);
        XtSetArg(args[n],XmNentryFontList,XmFontListCreate(fnt,SDC)); n++;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"columnheadings"))) {
        XmString *headings = (XmString *)XtCalloc(numcols,sizeof(XmString));

        tok = strtok(resource->val.cval,",");
        for ( i = 0; i < numcols; i++ ) {
            headings[i] = XmStringCreateLtoR(tok,SDC);
            if ( i == numcols - 2 )
                tok = strtok(NULL,"");
            else if ( i == numcols - 1 )
                ;
            else
                tok = strtok(NULL,",");
        }
        XtSetArg(args[n],XmNcolumnHeadings,headings); n++;
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"rowheadings"))) {
        XmString *headings = (XmString *)XtCalloc(numrows,sizeof(XmString));

        tok = strtok(resource->val.cval,",");
        for ( i = 0; i < numrows; i++ ) {
            headings[i] = XmStringCreateLtoR(tok,SDC);
            if ( i == numrows - 2 )
                tok = strtok(NULL,"");
            else if ( i == numrows - 1 )
                ;
            else
                tok = strtok(NULL,",");
        }
        XtSetArg(args[n],XmNrowHeadings,headings); n++;
    }
    tableW = XmCreateTable(widget,tablename,args,n);
    XtManageChild(tableW);
    if ( NULL != (resource = IndexResource(object,OBJECT,"rowvalue"))) {
		int rowno = 1;
		while ( resource ) {
			if ( !strcmp(resource->name,"rowvalue") && rowno <= numrows) {
                char **values = (char **)XtCalloc(numcols,sizeof(char *));
        
                tok = strtok(resource->val.cval,",");
                for ( i = 0; i < numcols; i++ ) {
					values[i] = XtMalloc(strlen(tok) + 1);
                    strcpy(values[i],tok);
                    if ( i == numcols - 2 )
                        tok = strtok(NULL,"");
                    else if ( i == numcols - 1 )
                        ;
                    else
                        tok = strtok(NULL,",");
                }
                XmTableSetRow(tableW,rowno,values); n++;
				rowno++;
            }
			resource = resource->next;
        }
    }
    return tableW;
}
