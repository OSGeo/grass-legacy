/**********************************************************************
   popup.c      - do popup's, popdown's, and getenv's
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

PopupEnviron(e)
    Environ *e;
{
    Shell *s = e->shells;
    Resource *resource;

    xgenGD.currentEnv = e;


    if ( (NULL != (resource = IndexResource((char *)e,ENVIRONMENT,"set"))) ||
         (NULL != (resource = IndexResource((char *)e,ENVIRONMENT,"runforeground"))) ||
         (NULL != (resource = IndexResource((char *)e,ENVIRONMENT,"runbackground")))) {
        while ( resource ) {
            if ( !strcmp(resource->name,"set") )
                DoSet((char *)e,resource,ENVIRONMENT,NULL);
            if ( !strncmp(resource->name,"run",3) )
                DoJob((char *)e,resource,ENVIRONMENT);

            resource = resource->next;
        }
    }

    /* Popup all initial shells */
    s = e->shells;
    while ( s ) {
        if ( s->initial ) 
            Popup_Shell(s);
        s = s->next;
    }
/***************************************************************
 *  check for existance of a sensitivity command....
 **************************************************************/
    if ( (NULL !=
          (resource = IndexResource((char *)e,ENVIRONMENT,"sensitive"))) ||
         (NULL !=
          (resource = IndexResource((char *)e,ENVIRONMENT,"insensitive")))) {
        while ( resource ) {
             if ( !strcmp(resource->name,"sensitive") ) {
                 EffectSensitivity(resource->val.cval,True);
             }
             if ( !strcmp(resource->name,"insensitive") ) {
                 EffectSensitivity(resource->val.cval,False);
             }
             resource = resource->next;
        }
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

    if ( (NULL != (resource = IndexResource((char *)s,SHELL,"set"))) ||
         (NULL != (resource = IndexResource((char *)s,SHELL,"runforeground"))) ||
         (NULL != (resource = IndexResource((char *)s,SHELL,"runbackground")))) {
        while ( resource ) {
            if ( !strcmp(resource->name,"set") )
                DoSet((char *)s,resource,SHELL,NULL);
            if ( !strncmp(resource->name,"run",3) )
                DoJob((char *)s,resource,SHELL);

            resource = resource->next;
        }
    }

/***************************************************************
 * If the widget doesn't exist create it.
 **************************************************************/
    if ( !s->widget ) {
        Create_Shell(s);
/***************************************************************
 * else destroy the old one and recreate it.
 **************************************************************/
    } else {
       Popdown_Shell(s);
       Create_Shell(s);
    }


/***************************************************************
 *  pop it up.
 **************************************************************/
    XtPopup(s->widget,XtGrabNone);
    s->popup = True;
/***************************************************************
 *  check for existance of a sensitivity command....
 **************************************************************/
    if ( (NULL !=
          (resource = IndexResource((char *)s,SHELL,"sensitive"))) ||
         (NULL !=
          (resource = IndexResource((char *)s,SHELL,"insensitive")))) {
        while ( resource ) {
             if ( !strcmp(resource->name,"sensitive") ) {
                 EffectSensitivity(resource->val.cval,True);
             }
             if ( !strcmp(resource->name,"insensitive") ) {
                 EffectSensitivity(resource->val.cval,False);
             }
             resource = resource->next;
        }
    }
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


