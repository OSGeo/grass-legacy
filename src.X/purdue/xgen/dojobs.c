#include "xgen.h"

DoJobs(ptr,type) 
	caddr_t ptr;
	int type;
{
	Resource *resource;
	char expanded[1024];
    Command *command = AllocCommand();
	InterfaceObject *object;
	Shell *shell;
	Environ *environ;

	switch(type) {
		case ENVIRONMENT:
			environ = (Environ *)ptr;
			resource = environ->resources;
			break;
		case SHELL:
			shell = (Shell *)ptr;
			resource = shell->resources;
			break;
		case OBJECT:
			object = (InterfaceObject *)ptr;
			resource = object->resources;
			break;
	}

/***************************************************************
 * now, search through the list from the beginning. Each time we 
 * get a commandarg resource we expand it and set the tail of the
 * argument list to the expanded string, this applies to the next
 * run command found....so ordering these is important. Each time
 * a run command is executed the commandarg list is set to NULL.
 * The capture output, and input from commands are dealt with 
 * in the same way.
 **************************************************************/

    while ( resource ) {
        if ( !strcmp(resource->name,"commandarg")) {
            /* set command arglist to include the result of expanding */

            sprintf(expanded,"%s",resource->val.cval);
    
            switch( ExpandString(expanded,1024) ) {
                case -1:
                    sprintf(errorbuf,
                        "a variable in command string [%s] is undefined\n",
                           resource->val.cval);
                       XgenWarning("commandarg procedure",errorbuf);
                    break;
                case 0:
                    command->arglist = (char *)XtMalloc(strlen(expanded)+1);
                    strcpy(command->arglist,expanded);
                    break;
                case 1:
                    sprintf(errorbuf,"command string [%s] has been truncated\n",
                           resource->val.cval);
                       XgenWarning("commandarg procedure",errorbuf);
                    break;
            }
        } else if ( !strcmp(resource->name,"inputfrom")) {
            command->input = True;
			if ( resource->variable ) ExpandVariable(resource);
            command->source = (char *)XtMalloc(strlen(resource->val.cval+1));
            strcpy(command->source,resource->val.cval);
        } else if ( !strcmp(resource->name,"captureoutput")) {
            command->capture = True;
			if ( resource->variable ) ExpandVariable(resource);
            command->sink = (char *)XtMalloc(strlen(resource->val.cval+1));
            strcpy(command->sink,resource->val.cval);
        } else if ( !strcmp(resource->name,"runbackground")) {
            /* set command arglist to include the result of expanding */
    
            sprintf(expanded,"%s",resource->val.cval);
    
            switch( ExpandString(expanded,1024) ) {
                case -1:
                    sprintf(errorbuf,
                        "a variable in command string [%s] is undefined\n",
                        resource->val.cval);
                    XgenWarning("runbackground procedure",errorbuf);
                    break;
                case 0:
                    command->path = 
                        (char *)XtMalloc(strlen(expanded)+1);
                    strcpy(command->path,expanded);
                    command->dowait = False;
                    DoExec(command);
                    break;
                case 1:
                    sprintf(errorbuf,"command string [%s] has been truncated\n",
                        resource->val.cval);
                    XgenWarning("runbackground procedure",errorbuf);
                    break;
            }
        } else if ( !strcmp(resource->name,"runforeground")) {
            /* set command arglist to include the result of expanding */
    
            sprintf(expanded,"%s",resource->val.cval);
    
            switch( ExpandString(expanded,1024) ) {
                case -1:
                    sprintf(errorbuf,
                        "a variable in command string [%s] is undefined\n",
                        resource->val.cval);
                    XgenWarning("runforeground procedure",errorbuf);
                    break;
                case 0:
                    command->path = 
                        (char *)XtMalloc(strlen(expanded)+1);
                    strcpy(command->path,expanded);
                    command->dowait = True;
                    DoExec(command);
                    break;
                case 1:
                    sprintf(errorbuf,"command string [%s] has been truncated\n",
                        resource->val.cval);
                    XgenWarning("runforeground procedure",errorbuf);
                    break;
            }
        }
        resource = resource->next;
    }
}
