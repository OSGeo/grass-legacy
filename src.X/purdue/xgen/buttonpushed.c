/***************************************************************
 * buttonpushed.c
 *
 * This file contains the callback associated with all buttons.
 **************************************************************/
#include "xgen.h"


/***************************************************************
 *  ButtonPushedCB - the callback associated with all buttons.
 **************************************************************/

void 
ButtonPushCB(w,cld,cad)
    Widget w;
    caddr_t cld;
    caddr_t cad;
{
/***************************************************************
 * the client data field is a pointer to the button's object information
 **************************************************************/
    InterfaceObject *object = (InterfaceObject *)cld;
/***************************************************************
 * the call data field is a pointer to the event causing the callback
 * to be called.
 **************************************************************/
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)cad;
    Resource *resource;
    Shell *shell;
    XmString xmLabel;
    char *text;
    int n;

    /***************************************************************
     * If the event field of the callback struct is NULL we were 
     * probably activated by a key press after tabbing to this 
     * button, we can't check for a shifted mouse click....
     * If the button press was shifted look for help and display it, else
     * ignore the buttonpress altogther, in either case return.
     **************************************************************/
    if ( cbs->event != NULL && cbs->event->xbutton.state & ShiftMask ) {
        if ( NULL != (resource = IndexResource(object,OBJECT,"help"))) {
            XmString help;
            if ( resource->variable ) ExpandVariable(resource);
            help = XmStringCreateLtoR(resource->val.cval,SDC);
            n = 0;
            XtSetArg(args[n],XmNmessageString,help); n++;
            XtSetValues(xgenGD.help,args,n);
            XtManageChild(xgenGD.help);
            XmStringFree(help);
        } 
        return;
    }
    /* get the buttons label text, and convert it to a character string */
    n = 0;
    XtSetArg(args[n],XmNlabelString,&xmLabel); n++;
    XtGetValues(w,args,n);

    XmStringGetLtoR(xmLabel,SDC,&text);

/***************************************************************
 * search for button procedures, 
 * Is the a set procedure present ? If so take care of all of them.
 **************************************************************/
    if ( NULL != (resource = IndexResource(object,OBJECT,"set"))) {
        DoSet(object,OBJECT,text);
    }

/***************************************************************
 *  check for existance of a run command....
 **************************************************************/
    if ( NULL != (resource = IndexResource(object,OBJECT,"runforeground"))
         || NULL != (resource = IndexResource(object,OBJECT,"runbackground"))) {
        DoJobs(object,OBJECT);
    }

/***************************************************************
 *  check for existance of a store command....
 **************************************************************/
    if ( NULL != (resource = IndexResource(object,OBJECT,"store"))) {
        char expanded[1024];

        sprintf(expanded,"%s",resource->val.cval);
        switch ( ExpandString(expanded,1024) ) {
            case -1:
                sprintf(errorbuf,
                    "a variable in store string [%s] is undefined\n",
                    resource->val.cval);
                XgenWarning("store procedure",errorbuf);
                break;
            case 0:
                XStoreBytes(xgenGD.display,expanded,strlen(expanded)); 
                break;
            case 1:
                sprintf(errorbuf,
                    "store string [%s] has been truncated\n",
                    resource->val.cval);
                XgenWarning("store procedure",errorbuf);
                break;
        }
    }

/***************************************************************
 *  check for existance of a clear command....
 **************************************************************/
    if ( NULL != (resource = IndexResource(object,OBJECT,"clear"))) {
        if ( resource->variable ) ExpandVariable(resource);
        /* first get object's shell to check if it's a command board */
        if ( NULL != (shell = IndexShellByObject(object))) {
            if ( shell->type == COMMANDBOARD ) {
                InterfaceObject *o = shell->objects;
                char *junk;
                char *obj;

                /* cycle thru the objects */
                while ( o ) {
                    /* copy the value or the clear resource */
                    junk = XtMalloc(strlen(resource->val.cval) + 1);
                    strcpy(junk,resource->val.cval);
                    /******************************************
                     * is it clear all objects or 
                     * is this object the one to clear ? 
                     ******************************************/
                    if(!strcmp(resource->val.cval,"all") || 
                       !strcmp(resource->val.cval,o->name)) {
                        /* determine the type and perform the clear  */
                        switch ( o->type ) {
                            case TEXTENTRY:
                                XmTextSetString(o->widget,"");
                                break;
                            case SLIDER:
                                XmScaleSetValue(o->widget,0);
                                break;
                            case TABLE:
                                ClearTable(o);
                                break;
                            case TOGGLE:
                                ClearToggle(o);
                                break;
                            case LIST:
                                XmListDeselectAllItems(o->widget);
                                ClearList(o->name);
                                break;
                        }
                    /* 
                     * else, if there is a paren in the resource value and
                     * the part before the paren is this object and this 
                     * object is a Table, clear the part of the table specified.
                     */
                    } else if ( (obj = strtok(junk,"(")) &&
                                !strcmp(obj,o->name) && o->type == TABLE) {
                         ClearTablePart(o,resource->val.cval);
                    }
                    XtFree(junk);

                    o = o->next;
                }
            } else {
    
                sprintf(errorbuf,"clear is ineffective in shell \"%s\"",
                    shell->name);
                XgenWarning("button pressed",errorbuf);
            }
        }
    }
    /* get first popup in the list ... */
    if ( NULL != (resource = IndexResource(object,OBJECT,"popup"))) {
        /* while not at the end of the resource list */
        while ( resource ) {

            if ( resource->variable ) ExpandVariable(resource);
            /* continue looking for other popup's */
            if ( !strcmp(resource->name,"popup") ) {
                /* got one, check for validity, and pop it up */
                if (NULL == (shell = IndexShell(resource->val.cval))) {
        
                    sprintf(errorbuf,"could not find shell \"%s\"",
                            resource->val.cval);
                    XgenWarning("popup shell",errorbuf);
                } else
                    Popup_Shell(shell);
            }
    
            resource = resource->next;
        }
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"popdown"))) {
        /* while not at the end of the resource list */
        while ( resource ) {

            if ( resource->variable ) ExpandVariable(resource);
            /* continue looking for other popdown's */
            if ( !strcmp(resource->name,"popdown") ) {
                if (NULL == (shell = IndexShell(resource->val.cval))) {
        
                    sprintf(errorbuf,"could not find shell \"%s\"",resource->val.cval);
                    XgenWarning("popdown shell",errorbuf);
                } else
                    Popdown_Shell(shell);
            }
            resource = resource->next;
        }
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"destroy"))) {
        /* while not at the end of the resource list */
        while ( resource ) {

            if ( resource->variable ) ExpandVariable(resource);
            /* continue looking for other destroy's */
            if ( !strcmp(resource->name,"destroy") ) {
                if (NULL == (shell = IndexShell(resource->val.cval))) {
        
                    sprintf(errorbuf,"could not find shell \"%s\"",
                            resource->val.cval);
                    XgenWarning("destroy shell",errorbuf);
                } else {
                    Popdown_Shell(shell);
                    XtDestroyWidget(shell->widget);
                    shell->widget = (Widget) 0;
                }
            }
            resource = resource->next;
        }
    }
    /* EXIT ??? */
    if ( NULL != (resource = IndexResource(object,OBJECT,"exit"))) {
        if ( resource->variable ) ExpandVariable(resource);
        XgenExit(resource->val.ival);
    }
    /* get a new environment */
    if ( NULL != (resource = IndexResource(object,OBJECT,"getenv"))) {
        Environ *e;

        /* popdown all currently popped up shells */
        PopdownAll(xgenGD.currentEnv);
        /***************************************************************
         * find the new environment and pop it up, 
         * making it the current environment 
         **************************************************************/
        if ( resource->variable ) ExpandVariable(resource);
        if ( NULL != (e = IndexEnviron(resource->val.cval))) {
            PopupEnviron(e);
            xgenGD.currentEnv = e;
        } else {

            sprintf(errorbuf,"could not find environment \"%s\"",
                resource->val.cval);
            XgenWarning("get environment",errorbuf);
        }
    }
}

