/***************************************************************
 * allocypes.c
 *
 *
 **************************************************************/
#include "xgen.h"

char *
SaveString(s)
    char *s;
{
    char *cp = (char *)XtMalloc(strlen(s) + 1);
    
    if ( cp ) {
        strcpy(cp,s);
        return(cp);
    }
    XgenFatalError("SaveString","out of memory");
}

Resource *
AllocResource()
{
    Resource *resource = (Resource *)XtMalloc(sizeof(Resource));

    if ( resource ) {
		bzero((char *)resource,sizeof(Resource));
        return resource;
	}
    XgenFatalError("AllocResource","out of memory");
}


InterfaceObject *
AllocObject()
{
    InterfaceObject *object = (InterfaceObject *)XtMalloc(sizeof(InterfaceObject));
    if ( object ) {
		bzero((char *)object,sizeof(InterfaceObject));
        return object;
	}
    XgenFatalError("AllocObject","out of memory");
}


Shell *
AllocShell()
{
    Shell *shell = (Shell *)XtMalloc(sizeof(Shell));
    if ( shell ) {
		bzero((char *)shell,sizeof(Shell));
        return shell;
	}
    XgenFatalError("AllocShell","out of memory");
}


Environ *
AllocEnviron()
{
    Environ *environ = (Environ *)XtMalloc(sizeof(Environ));
    if ( environ ) {
		bzero((char *)environ,sizeof(Environ));
        return environ;
	}
    XgenFatalError("AllocEnviron","out of memory");
}



Command *
AllocCommand()
{
    Command *command = (Command *)XtMalloc(sizeof(Command));
    if ( command ) {
		bzero((char *)command,sizeof(Command));
        return command;
	}
    XgenFatalError("AllocCommand","out of memory");
}


