#include "xgen.h"

PopupEnviron(e)
    Environ *e;
{
    Shell *s = e->shells;
    Resource *resource;

    /* do foreground or background commands */
    if ( NULL != (resource = IndexResource(e,ENVIRONMENT,"runforeground")) ||
         NULL != (resource = IndexResource(e,ENVIRONMENT,"runbackground"))) {
		 DoJobs(e,ENVIRONMENT);
    }

    /* do set commands */
    if ( NULL != (resource = IndexResource(e,ENVIRONMENT,"set"))) {
		 DoSet(e,ENVIRONMENT,NULL);
    }


    /* Create all uncreated shells 
	KAB DO NOT CREATE ALL UNCREATED SHELLS, THEY MIGHT BE ORDER DEPENDENT !!
    while ( s ) {
        if ( !s->widget ) 
            Create_Shell(s);
        s = s->next;
    }
	*/

    /* Popup all initial shells */
    s = e->shells;
    while ( s ) {
        if ( s->initial ) 
            Popup_Shell(s);
        s = s->next;
    }
}

PopdownAll(e)
    Environ *e;
{
    Shell *s = e->shells;

    while ( s ) {
        if ( s->popup ) {
            s->popup = False;
			if ( s->widget ) XtPopdown(s->widget);
        }

        s = s->next;
    }
    
}

Popup_Shell(s)
    Shell *s;
{
    Resource *resource;

    if ( !s->widget )
        Create_Shell(s);
    s->popup = True;
    /* do foreground or background commands */
    if ( NULL != (resource = IndexResource(s,SHELL,"runforeground")) ||
         NULL != (resource = IndexResource(s,SHELL,"runbackground"))) {
		 DoJobs(s,SHELL);
    }

    /* do set commands */
    if ( NULL != (resource = IndexResource(s,SHELL,"set"))) {
		 DoSet(s,SHELL,NULL);
    }
    XtPopup(s->widget,XtGrabNone);
}

Popdown_Shell(s)
    Shell *s;
{
    if ( s->widget ) XtPopdown(s->widget);
    s->popup = False;
}
