/***************************************************************
 * allocypes.c
 * This file contains most of the memeory allocation routines.
 **************************************************************/
#include "xgen.h"


/***************************************************************
 * SaveString -maker a copy of the input string and return 
 * a pointer to it.
 **************************************************************/
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

/***************************************************************
 * AllocResource - allocate a resource node, zero it out,
 * and return the pointer to it.
 **************************************************************/
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

/***************************************************************
 * AllocObject - allocate an object node, zero it out,
 * and return the pointer to it.
 **************************************************************/
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

/***************************************************************
 * AllocShell - allocate a shell node, zero it out,
 * and return the pointer to it.
 **************************************************************/
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

/***************************************************************
 * AllocEnviron - allocate an environ node, zero it out,
 * and return the pointer to it.
 **************************************************************/
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

/***************************************************************
 * AllocCommand - allocate a command node, zero it out,
 * and return the pointer to it.
 **************************************************************/
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


