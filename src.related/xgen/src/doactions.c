/**********************************************************************
   doactions.c      - perform action commands
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
DoActions(object, text ) 
    InterfaceObject *object;
    char *text;
{
    Resource                       *resource;
    Shell                          *shell;

    /***************************************************************
     * search for button procedures,
     * check for existance of a run command....
     * Is the a set procedure present ? If so take care of all of them.
     **************************************************************/
    if ((NULL != (resource = IndexResource((char *) object, OBJECT, "set"))) ||
            (NULL != (resource = IndexResource((char *) object, OBJECT, "updateobject"))) ||
            (NULL != (resource = IndexResource((char *) object, OBJECT, "postnotice"))) ||
            (NULL != (resource = IndexResource((char *) object, OBJECT, "runforeground"))) ||
            (NULL != (resource = IndexResource((char *) object, OBJECT, "runbackground")))) {
        resource = object->resources;

        while (resource) {
            if (!strcmp(resource->name, "updateobject"))
                UpdateObjectValue(resource);
            if (!strcmp(resource->name, "postnotice"))
                DoNotice((char *) object, resource, OBJECT);
            if (!strcmp(resource->name, "set"))
                DoSet((char *) object, resource, OBJECT, text);
            if (!strncmp(resource->name, "run", 3))
                DoJob((char *) object, resource, OBJECT);

            resource = resource->next;
        }
    }
    /***************************************************************
     *  check for existance of a sensitivity command....
     **************************************************************/
    if ((NULL !=
         (resource = IndexResource((char *) object, OBJECT, "sensitive"))) ||
            (NULL !=
      (resource = IndexResource((char *) object, OBJECT, "insensitive")))) {
        resource = object->resources;

        while (resource) {
            if (!strcmp(resource->name, "sensitive")) {
	       if (resource->variable)
		    ExpandVariable(resource, (char *) object, OBJECT);
                EffectSensitivity(resource->val.cval, True);
            }
            if (!strcmp(resource->name, "insensitive")) {
	       if (resource->variable)
		    ExpandVariable(resource, (char *) object, OBJECT);
                EffectSensitivity(resource->val.cval, False);
            }
            resource = resource->next;
        }
    }
    /***************************************************************
     *  check for existance of a store command....
     **************************************************************/
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "store"))) {
        char                            expanded[8192];

        sprintf(expanded, "%s", resource->val.cval);
        switch (ExpandString(expanded, 8192)) {
        case -1:
            sprintf(errorbuf,
                    "a variable in store string [%s] is undefined\n",
                    resource->val.cval);
            XgenWarning("store procedure", errorbuf);
            break;
        case 0:
            XStoreBytes(xgenGD.display, expanded, strlen(expanded));
            break;
        case 1:
            sprintf(errorbuf,
                    "store string [%s] has been truncated\n",
                    resource->val.cval);
            XgenWarning("store procedure", errorbuf);
            break;
        }
    }
    /***************************************************************
     *  check for existance of a clear command....
     **************************************************************/
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "clear"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        /* first get object's shell to check if it's a command board */
        if (NULL != (shell = IndexShellByObject(object))) {
            if (shell->type == COMMANDBOARD) {
                InterfaceObject                *o = shell->objects;
                char                           *junk;
                char                           *obj;

                /* cycle thru the objects */
                while (o) {
                    /* copy the value or the clear resource */
                    junk = XtMalloc(strlen(resource->val.cval) + 1);
                    strcpy(junk, resource->val.cval);
                    /******************************************
                     * is it clear all objects or
                     * is this object the one to clear ?
                     ******************************************/
                    if (!strcmp(resource->val.cval, "all") ||
                            !strcmp(resource->val.cval, o->name)) {
                        /* determine the type and perform the clear  */
                        switch (o->type) {
                        case TEXTENTRY:
                            XmTextSetString(o->widget, "");
                            break;
                        case SLIDER:
                            XmScaleSetValue(o->widget, 0);
                            break;
                        case TABLE:
                            ClearTable(o);
                            break;
                        case TOGGLE:
                            ClearToggle(o);
                            break;
                        case LIST:
                            XmListDeselectAllItems(o->widget);
                            ClearListObject(o->name);
                            break;
                        case MULTILINE:
                            XmTextSetString(o->widget, "");
                            break;
                        }
                        /*
                         * else, if there is a paren in the resource value
                         * and the part before the paren is this object and
                         * this object is a Table, clear the part of the
                         * table specified.
                         */
                    } else if ((obj = (char *) strtok(junk, "(")) &&
                               !strcmp(obj, o->name) && o->type == TABLE) {
                        ClearTablePart(o, resource->val.cval);
                    }
                    XtFree(junk);

                    o = o->next;
                }
            } else {

                sprintf(errorbuf, "clear is ineffective in shell \"%s\"",
                        shell->name);
                XgenWarning("button pressed", errorbuf);
            }
        }
    }
    /* get first popup in the list ... */
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "popup"))) {
        /* while not at the end of the resource list */
        while (resource) {

            if (resource->variable)
                ExpandVariable(resource, (char *) object, OBJECT);
            /* continue looking for other popup's */
            if (!strcmp(resource->name, "popup")) {
                /* got one, check for validity, and pop it up */
                if (NULL == (shell = IndexShell(resource->val.cval))) {

                    sprintf(errorbuf, "could not find shell \"%s\"",
                            resource->val.cval);
                    XgenWarning("popup shell", errorbuf);
                } else
                    Popup_Shell(shell);
            }
            resource = resource->next;
        }
    }
    /* get a new environment */
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "getenv"))) {
        Environ                        *e;
        Environ                        *eTmp = xgenGD.currentEnv;

        /***************************************************************
         * find the new environment and pop it up,
         * making it the current environment
         **************************************************************/
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (NULL != (e = IndexEnviron(resource->val.cval))) {
            PopupEnviron(e);
	    /* popdown all currently popped up shells */
	    PopdownAll(eTmp);
            xgenGD.currentEnv = e;
        } else {
	    Boolean save = verbose;

	    verbose = True;

            sprintf(errorbuf, "could not find environment \"%s\"",
                    resource->val.cval);
            XgenWarning("get environment", errorbuf);
	    verbose = save;
        }
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "popdown"))) {
        /* while not at the end of the resource list */
        while (resource) {

            if (resource->variable)
                ExpandVariable(resource, (char *) object, OBJECT);
            /* continue looking for other popdown's */
            if (!strcmp(resource->name, "popdown")) {
                if (NULL == (shell = IndexShell(resource->val.cval))) {

                    sprintf(errorbuf, "could not find shell \"%s\"", resource->val.cval);
                    XgenWarning("popdown shell", errorbuf);
                } else
                    PopdownShell(shell);
            }
            resource = resource->next;
        }
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "destroy"))) {
        /* while not at the end of the resource list */
        while (resource) {

            if (resource->variable)
                ExpandVariable(resource, (char *) object, OBJECT);
            /* continue looking for other destroy's */
            if (!strcmp(resource->name, "destroy")) {
                if (NULL == (shell = IndexShell(resource->val.cval))) {

                    sprintf(errorbuf, "could not find shell \"%s\"",
                            resource->val.cval);
                    XgenWarning("destroy shell", errorbuf);
                } else {
                    PopdownShell(shell);
                    XtDestroyWidget(shell->widget);
                    shell->widget = (Widget) 0;
                }
            }
            resource = resource->next;
        }
    }
    /* EXIT ??? */
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "exit"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        XgenExit(resource->val.ival);
    }
}
