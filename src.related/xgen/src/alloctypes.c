/**********************************************************************
   alloctypes.c - allocate a node for Xgen's internal hierarchy
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


/***************************************************************
 * SaveString -maker a copy of the input string and return
 * a pointer to it.
 **************************************************************/
char                           *
SaveString(s)
    char                           *s;
{
    char                           *cp = (char *) XtMalloc(strlen(s) + 1);

    if (cp) {
        strcpy(cp, s);
        return (cp);
    }
    XgenFatalError("SaveString", "out of memory");
    return (NULL);
}

/***************************************************************
 * AllocWidgetListElement - allocate a widget list element node,
 * zero it out, and return the pointer to it.
 **************************************************************/
WidgetListElement              *
AllocWidgetListElement()
{
    WidgetListElement              *widgetListElement =
    (WidgetListElement *) XtMalloc(sizeof(WidgetListElement));

    if (widgetListElement) {
        bzero((char *) widgetListElement, sizeof(WidgetListElement));
        return widgetListElement;
    }
    XgenFatalError("AllocWidgetListElement", "out of memory");
    return (NULL);
}

/***************************************************************
 * AllocResource - allocate a resource node, zero it out,
 * and return the pointer to it.
 **************************************************************/
Resource                       *
AllocResource()
{
    Resource                       *resource = (Resource *) XtMalloc(sizeof(Resource));

    if (resource) {
        bzero((char *) resource, sizeof(Resource));
        return resource;
    }
    XgenFatalError("AllocResource", "out of memory");
    return (NULL);
}

/***************************************************************
 * AllocObject - allocate an object node, zero it out,
 * and return the pointer to it.
 **************************************************************/
InterfaceObject                *
AllocObject()
{
    InterfaceObject                *object = (InterfaceObject *) XtMalloc(sizeof(InterfaceObject));
    if (object) {
        bzero((char *) object, sizeof(InterfaceObject));
        return object;
    }
    XgenFatalError("AllocObject", "out of memory");
    return (NULL);
}

/***************************************************************
 * AllocShell - allocate a shell node, zero it out,
 * and return the pointer to it.
 **************************************************************/
Shell                          *
AllocShell()
{
    Shell                          *shell = (Shell *) XtMalloc(sizeof(Shell));
    if (shell) {
        bzero((char *) shell, sizeof(Shell));
        return shell;
    }
    XgenFatalError("AllocShell", "out of memory");
    return (NULL);
}

/***************************************************************
 * AllocEnviron - allocate an environ node, zero it out,
 * and return the pointer to it.
 **************************************************************/
Environ                        *
AllocEnviron()
{
    Environ                        *environ = (Environ *) XtMalloc(sizeof(Environ));
    if (environ) {
        bzero((char *) environ, sizeof(Environ));
        return environ;
    }
    XgenFatalError("AllocEnviron", "out of memory");
    return (NULL);
}

/***************************************************************
 * AllocCommand - allocate a command node, zero it out,
 * and return the pointer to it.
 **************************************************************/
Command                        *
AllocCommand()
{
    Command                        *command = (Command *) XtMalloc(sizeof(Command));
    if (command) {
        bzero((char *) command, sizeof(Command));
        return command;
    }
    XgenFatalError("AllocCommand", "out of memory");
    return (NULL);
}
