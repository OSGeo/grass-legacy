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

void
PopupEnviron(e)
    Environ                        *e;
{
    Shell                          *s = e->shells;
    Resource                       *resource;

    xgenGD.currentEnv = e;

    if ((NULL != (resource = IndexResource((char *) e, ENVIRONMENT, "set"))) ||
            (NULL != (resource = IndexResource((char *) e, ENVIRONMENT, "updateobject"))) ||
            (NULL != (resource = IndexResource((char *) e, ENVIRONMENT, "postnotice"))) ||
            (NULL != (resource = IndexResource((char *) e, ENVIRONMENT, "runforeground"))) ||
            (NULL != (resource = IndexResource((char *) e, ENVIRONMENT, "interactive"))) ||
            (NULL != (resource = IndexResource((char *) e, ENVIRONMENT, "runbackground")))) {
        resource = e->resources;

        while (resource) {
            if (!strcmp(resource->name, "set")) {
                DoSet((char *) e, resource, ENVIRONMENT, NULL);
            }
            if (!strcmp(resource->name, "postnotice")) {
                DoNotice((char *) e, resource, ENVIRONMENT);
            }
            if (!strncmp(resource->name, "run", 3)) {
                DoJob((char *) e, resource, ENVIRONMENT);
            }
            if (!strcmp(resource->name, "updateobject")) {
                UpdateObjectValue(resource);
            }
            if (!strcmp(resource->name, "interactive")) {
                DoJob((char *) e, resource, ENVIRONMENT);
            }
            resource = resource->next;
        }
    }
    /* Popup all initial shells */
    s = e->shells;
    while (s) {
        if (s->initial)
            Popup_Shell(s);
        s = s->next;
    }
    /***************************************************************
     *  check for existance of a sensitivity command....
     **************************************************************/
    if ((NULL !=
         (resource = IndexResource((char *) e, ENVIRONMENT, "sensitive"))) ||
            (NULL !=
      (resource = IndexResource((char *) e, ENVIRONMENT, "insensitive")))) {
        resource = e->resources;

        while (resource) {
            if (!strcmp(resource->name, "sensitive")) {
	       if (resource->variable)
		    ExpandVariable(resource, (char *) e, OBJECT);
                EffectSensitivity(resource->val.cval, True);
            }
            if (!strcmp(resource->name, "insensitive")) {
	       if (resource->variable)
		    ExpandVariable(resource, (char *) e, OBJECT);
                EffectSensitivity(resource->val.cval, False);
            }
            resource = resource->next;
        }
    }
}

void
PopdownAll(e)
    Environ                        *e;
{
    Shell                          *s = e->shells;

    while (s) {
        if (s->popup)
            PopdownShell(s);
        s = s->next;
    }
}

void
Popup_Shell(s)
    Shell                          *s;
{
    Resource                       *resource;

    if (s->popup)
        return;
    /***************************************************************
     * If the shell is not in the current environment, then print
     * a warning and return.
     **************************************************************/
    if (!ShellInCurrentEnviron(s)) {

        sprintf(errorbuf, "shell [%s] not in current environment\n", s->name);
        XgenWarning("popup shell", errorbuf);
        return;
    }
    if ((NULL != (resource = IndexResource((char *) s, SHELL, "set"))) ||
            (NULL != (resource = IndexResource((char *) s, SHELL, "updateobject"))) ||
            (NULL != (resource = IndexResource((char *) s, SHELL, "postnotice"))) ||
            (NULL != (resource = IndexResource((char *) s, SHELL, "runforeground"))) ||
    (NULL != (resource = IndexResource((char *) s, SHELL, "interactive"))) ||
            (NULL != (resource = IndexResource((char *) s, SHELL, "runbackground")))) {
        resource = s->resources;

        while (resource) {
            if (!strcmp(resource->name, "updateobject")) {
                UpdateObjectValue(resource->val.cval);
            }
            if (!strcmp(resource->name, "set")) {
                DoSet((char *) s, resource, SHELL, NULL);
            }
            if (!strcmp(resource->name, "postnotice")) {
                DoNotice((char *) s, resource, SHELL);
            }
            if (!strncmp(resource->name, "run", 3)) {
                DoJob((char *) s, resource, SHELL);
            }
            if (!strcmp(resource->name, "interactive")) {
                DoJob((char *) s, resource, SHELL);
            }
            resource = resource->next;
        }
    }
    /***************************************************************
     * If the widget doesn't exist create it.
     **************************************************************/
    if (!s->widget) {
        Create_Shell(s);
        /***************************************************************
         * else destroy the old one and recreate it.
         **************************************************************/
    } else {
        PopdownShell(s);
        Create_Shell(s);
    }

    /***************************************************************
     *  if we are not using the control box, but the user had popped
     *  down all of our shells and since we are popping one up let's
     *  unmap the control box...
     **************************************************************/
    if ( nocontrol && AllPoppedDown() )  {
	if ( xgenGD.toplevelIsMapped )
	    XtUnmapWidget(xgenGD.applShell);
    }
    /***************************************************************
     *  pop it up.
     **************************************************************/
    XtPopup(s->widget, XtGrabNone);
    s->popup = True;
    /***************************************************************
     *  check for existance of a sensitivity command....
     **************************************************************/
    if ((NULL !=
         (resource = IndexResource((char *) s, SHELL, "sensitive"))) ||
            (NULL !=
             (resource = IndexResource((char *) s, SHELL, "insensitive")))) {
        resource = s->resources;

        while (resource) {
            if (!strcmp(resource->name, "sensitive")) {
	       if (resource->variable)
		    ExpandVariable(resource, (char *) s, OBJECT);
                EffectSensitivity(resource->val.cval, True);
            }
            if (!strcmp(resource->name, "insensitive")) {
	       if (resource->variable)
		    ExpandVariable(resource, (char *) s, OBJECT);
                EffectSensitivity(resource->val.cval, False);
            }
            resource = resource->next;
        }
    }
}

void
PopdownShell(s)
    Shell                          *s;
{
    InterfaceObject                *o = s->objects;

    if (s->widget) {
        XtPopdown(s->widget);
        XtDestroyWidget(s->widget);
        /***************************************************************
         * If any list or toggle objects are in this shell we must clean
         * them up.
         **************************************************************/
        while (o) {
            switch (o->type) {
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
    s->widget = (Widget) 0;
    if (s->dynObjects) {
        FreeWidgetList(s->dynObjects);
    }
    s->dynObjects = (WidgetListElement *) 0;
    /**************************************************************
     * if we are not using the control box and the user has popped 
     * down all shells, let's pop up the control box so they can
     * quit or start a new environment...
     **************************************************************/
    if ( nocontrol && AllPoppedDown() ) {
	XtMapWidget(xgenGD.applShell);
	xgenGD.toplevelIsMapped = True;
    }
}

int
AllPoppedDown()
{
    Environ *e = xgenGD.toplevelEnv;

    while ( e ) {
	Shell *s = e->shells;
	while ( s ) {
	    if ( s->popup ) return 0;
	    s = s->next;
	}
	e = e->next;
    }
    return 1;
}

void
FreeWidgetList(wl)
    WidgetListElement              *wl;
{
    if (wl->next != NULL) {
        FreeWidgetList(wl->next);
    }
    XtFree((char *)wl);
}
