#include "xgen.h"


void 
ButtonPushCB(w,cld,cad)
    Widget w;
    caddr_t cld;
    caddr_t cad;
{
    InterfaceObject *object = (InterfaceObject *)cld;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)cad;
    Resource *resource;
    Shell *shell;
    XmString xmLabel;
    char *text;
    int n;

    /***************************************************************
     * If the event field of the callback struct isd NULL we were 
     * probably activated by a key press after tabbing to this 
     * button, we can't check for a shifted mouse click....
     * If the button press was shifted look for help and display it, else
     * ignore the buttonpress altogther, in either case return.
     **************************************************************/
    if ( cbs->event != NULL && cbs->event->xbutton.state & ShiftMask ) {
        if ( NULL != (resource = IndexResource(object,OBJECT,"help"))) {
            XmString help;
            help = XmStringCreateLtoR(resource->val.cval,SDC);
            n = 0;
            XtSetArg(args[n],XmNmessageString,help); n++;
            XtSetValues(xgenGD.help,args,n);
            XtManageChild(xgenGD.help);
			XmStringFree(help);
        } 
        return;
    }
    n = 0;
    XtSetArg(args[n],XmNlabelString,&xmLabel); n++;
    XtGetValues(w,args,n);

    XmStringGetLtoR(xmLabel,SDC,&text);
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
    if ( NULL != (resource = IndexResource(object,OBJECT,"store"))) {
        /* KAB: this should store text into a cut buffer */
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"clear"))) {
        /* first get object's shell to check if it's a command board */
        if ( NULL != (shell = IndexShellByObject(object))) {
            if ( shell->type == COMMANDBOARD ) {
                InterfaceObject *o = shell->objects;
                char *junk;
                char *obj;

                while ( o ) {
                    junk = XtMalloc(strlen(resource->val.cval) + 1);
                    strcpy(junk,resource->val.cval);
                    if(!strcmp(resource->val.cval,"all") || 
                       !strcmp(resource->val.cval,o->name)) {
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
                char errorbuf[80];
    
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

            /* continue looking for other popup's */
            if ( !strcmp(resource->name,"popup") ) {
                /* got one, check for validity, and pop it up */
                if (NULL == (shell = IndexShell(resource->val.cval))) {
                    char errorbuf[80];
        
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

            /* continue looking for other popdown's */
            if ( !strcmp(resource->name,"popdown") ) {
                if (NULL == (shell = IndexShell(resource->val.cval))) {
                    char errorbuf[80];
        
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

            /* continue looking for other destroy's */
            if ( !strcmp(resource->name,"destroy") ) {
                if (NULL == (shell = IndexShell(resource->val.cval))) {
                    char errorbuf[80];
        
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
    if ( NULL != (resource = IndexResource(object,OBJECT,"exit"))) {
		if ( resource->variable ) ExpandVariable(resource);
        XgenExit(resource->val.ival);
    }
    if ( NULL != (resource = IndexResource(object,OBJECT,"getenv"))) {
        Environ *e;

        PopdownAll(xgenGD.currentEnv);
        if ( NULL != (e = IndexEnviron(resource->val.cval))) {
            PopupEnviron(e);
            xgenGD.currentEnv = e;
        } else {
            char errorbuf[80];

            sprintf(errorbuf,"could not find environment \"%s\"",
                resource->val.cval);
            XgenWarning("get environment",errorbuf);
        }
    }
}

