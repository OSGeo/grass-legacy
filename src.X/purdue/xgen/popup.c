#include "xgen.h"

PopupEnviron(e)
    Environ *e;
{
    Shell *s = e->shells;
    Resource *resource;

	xgenGD.currentEnv = e;

    /* do foreground or background commands */
    if ( NULL != (resource = IndexResource(e,ENVIRONMENT,"runforeground")) ||
         NULL != (resource = IndexResource(e,ENVIRONMENT,"runbackground"))) {
		if ( resource->variable ) ExpandVariable(resource);
		 DoJobs(e,ENVIRONMENT);
    }

    /* do set commands */
    if ( NULL != (resource = IndexResource(e,ENVIRONMENT,"set"))) {
		if ( resource->variable ) ExpandVariable(resource);
		 DoSet(e,ENVIRONMENT,NULL);
    }

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
        if ( s->popup ) 
			Popdown_Shell(s);
        s = s->next;
    }
    
}

Popup_Shell(s)
    Shell *s;
{
    Resource *resource;

/***************************************************************
 * If the shell is not in the current environment, then print
 * a warning and return.
 **************************************************************/
	if ( !ShellInCurrentEnviron(s) ) {

		sprintf(errorbuf,"shell [%s] not in current environment\n",s->name);
		XgenWarning("popup shell",errorbuf);
		return;
	}

/***************************************************************
 * If the widget doesn't exist create it.
 **************************************************************/
    if ( !s->widget ) {
        Create_Shell(s);
/***************************************************************
 * else destroy the old one and recreate it.
 **************************************************************/
	}


/***************************************************************
 * do foreground or background commands 
 **************************************************************/
    if ( NULL != (resource = IndexResource(s,SHELL,"runforeground")) ||
         NULL != (resource = IndexResource(s,SHELL,"runbackground"))) {
		if ( resource->variable ) ExpandVariable(resource);
		 DoJobs(s,SHELL);
    }

/***************************************************************
 * do set commands 
 **************************************************************/
    if ( NULL != (resource = IndexResource(s,SHELL,"set"))) {
		if ( resource->variable ) ExpandVariable(resource);
		 DoSet(s,SHELL,NULL);
    }
/***************************************************************
 *  pop it up.
 **************************************************************/
    XtPopup(s->widget,XtGrabNone);
    s->popup = True;
}

Popdown_Shell(s)
    Shell *s;
{
	InterfaceObject *o = s->objects;

    if ( s->widget ) {
		XtPopdown(s->widget);
	    XtDestroyWidget(s->widget);
        /***************************************************************
         * If any list or toggle objects are in this shell we must clean 
		 * them up.
         **************************************************************/
		while ( o ) {
			switch ( o->type ) {
				case LIST:
					DeleteListInfo(o->name);
					break;
				case TOGGLE:
					DeleteToggleInfo(o->name);
					break;
			}
			o = o->next;
		}
	}
    s->popup = False;
    s->widget = (Widget)0;
}


