#include "xgen.h"

DoSet(ptr,type,text)
    caddr_t ptr;
    int type;
	char *text;
{
    Resource *resource;
	Environ *environ;
	Shell *shell;
	InterfaceObject *object;
	char expanded[1024];
	char buf[1024];
	char *envPtr;

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

    while ( resource ) {
        if ( !strcmp(resource->name,"set")) {
    
        /***************************************************************
         * value will be in x=y form, check for x=buttonname for dynamic
         * assignment of the object->name to the variable.
         * Also, expand the string if neccessary.
         **************************************************************/
            
            sprintf(expanded,"%s",resource->val.cval);
    
            switch( ExpandString(expanded,1024) ) {
                case -1:
                    sprintf(errorbuf,
                        "a variable in set string [%s] is undefined\n",
                        resource->val.cval);
                    XgenWarning("button pressed",errorbuf);
                    break;
                case 0:
                    if ( !strncmp(rindex(expanded,'=')+1,"!",1)) {
                        FILE *fp;

                        if ( NULL == (fp = popen(rindex(expanded,'=')+2,"r"))) {

                            sprintf(errorbuf,
                                "<%s> popen(command) failed", expanded);
                            XgenWarning("set procedure",errorbuf);
                        } else {
                            char junk;
                        /**************************************************
                         * get the first 1024 chars, truncate if longer.
                         *************************************************/
                            fgets(buf,1024,fp);
                            fclose(fp);
                            pclose(fp);
                            ReplaceButtonName(expanded,buf);
                            envPtr = XtMalloc(strlen(expanded)+1);
                            strcpy(envPtr,expanded);
                            putenv(envPtr);
                        }
                    } else {
                        if (!strcmp(rindex(expanded,'=')+1,"buttonname")) 
                            ReplaceButtonName(expanded,text);
                        envPtr = XtMalloc(strlen(expanded)+1);
                        strcpy(envPtr,expanded);
                        putenv(envPtr);
                    }
                    break;
                case 1:
                    sprintf(errorbuf,"set string [%s] has been truncated\n",
                        resource->val.cval);
                    XgenWarning("button pressed",errorbuf);
                    break;
            }
    
        }
        resource = resource->next;
    }
}
