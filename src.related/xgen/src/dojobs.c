/**********************************************************************
   dojobs.c     - process a shell command before execution
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
DoJob(ptr, inresource, type)
    caddr_t                         ptr;
    Resource                       *inresource;
    int                             type;
{
    Resource                       *resource = NULL;
    char                           *savedResourceString = NULL;
    Boolean                         objectExpanded = False;
    Command                        *command = AllocCommand();
    InterfaceObject                *object;
    Shell                          *shell;
    Environ                        *environ;

    switch (type) {
    case ENVIRONMENT:
        environ = (Environ *) ptr;
        command->shell = xgenGD.applShell;
        resource = environ->resources;
        break;
    case SHELL:
        shell = (Shell *) ptr;
        command->shell = shell->widget;
        resource = shell->resources;
        break;
    case OBJECT:
        object = (InterfaceObject *) ptr;
        command->shell = XtParent(object->widget);
        resource = object->resources;
        break;
    }

    while (resource) {
        if (!strcmp(resource->name, "commandarg")) {
            /* set command arglist to include the result of expanding */

            if (resource->variable)
                ExpandVariable(resource, (char *) ptr, type);
            if (resource->val.cval == NULL) {
                fprintf(stderr, "\tignoring command\n\n");
                return;
            }
            if (strrchr(resource->val.cval, '[')) {
                savedResourceString = XtMalloc(strlen(resource->val.cval) + 1);
                strcpy(savedResourceString, resource->val.cval);
                ExpandObjectValues(resource, ptr, type);
                objectExpanded = True;
            }
            command->arglist = (char *) XtMalloc(strlen(resource->val.cval) + 1);
            strcpy(command->arglist, resource->val.cval);
            if (objectExpanded) {
                XtFree(resource->val.cval);
                resource->val.cval = (char *)
                    XtMalloc(strlen(savedResourceString) + 1);
                strcpy(resource->val.cval, savedResourceString);
                XtFree(savedResourceString);
                objectExpanded = False;
            }
        } else if (!strcmp(resource->name, "inputfrom")) {
            command->input = True;
            if (resource->variable)
                ExpandVariable(resource, (char *) ptr, type);
            if (resource->val.cval == NULL) {
                fprintf(stderr, "\tignoring command\n\n");
                return;
            }
            if (strrchr(resource->val.cval, '[')) {
                savedResourceString = XtMalloc(strlen(resource->val.cval) + 1);
                strcpy(savedResourceString, resource->val.cval);
                ExpandObjectValues(resource, ptr, type);
                objectExpanded = True;
            }
            command->source = (char *) XtMalloc(strlen(resource->val.cval) + 1);
            strcpy(command->source, resource->val.cval);
            if (objectExpanded) {
                XtFree(resource->val.cval);
                resource->val.cval = (char *)
                    XtMalloc(strlen(savedResourceString) + 1);
                strcpy(resource->val.cval, savedResourceString);
                XtFree(savedResourceString);
                objectExpanded = False;
            }
        } else if (!strcmp(resource->name, "notify")) {
            if (resource->variable)
                ExpandVariable(resource, (char *) ptr, type);
            command->notify = resource->val.bval;
        } else if (!strcmp(resource->name, "captureoutput")) {
            command->capture = True;
            if (resource->variable)
                ExpandVariable(resource, (char *) ptr, type);
            if (resource->val.cval == NULL) {
                fprintf(stderr, "\tignoring command\n\n");
                return;
            }
            if (strrchr(resource->val.cval, '[')) {
                savedResourceString = XtMalloc(strlen(resource->val.cval) + 1);
                strcpy(savedResourceString, resource->val.cval);
                ExpandObjectValues(resource, ptr, type);
                objectExpanded = True;
            }
            command->sink = (char *) XtMalloc(strlen(resource->val.cval) + 1);
            strcpy(command->sink, resource->val.cval);
            if (objectExpanded) {
                XtFree(resource->val.cval);
                resource->val.cval = (char *)
                    XtMalloc(strlen(savedResourceString) + 1);
                strcpy(resource->val.cval, savedResourceString);
                XtFree(savedResourceString);
                objectExpanded = False;
            }
        } else if (!strcmp(resource->name, "commandshell")) {
            if (resource->variable)
                ExpandVariable(resource, (char *) ptr, type);
            if (resource->val.cval == NULL) {
                fprintf(stderr, "\tignoring command\n\n");
                return;
            }
            if (strrchr(resource->val.cval, '[')) {
                savedResourceString = XtMalloc(strlen(resource->val.cval) + 1);
                strcpy(savedResourceString, resource->val.cval);
                ExpandObjectValues(resource, ptr, type);
                objectExpanded = True;
            }
            bzero((char *) command, sizeof(Command));
            command->cinterp = (char *) XtMalloc(strlen(resource->val.cval) + 1);
            strcpy(command->cinterp, resource->val.cval);
            command->interp = True;
            if (objectExpanded) {
                XtFree(resource->val.cval);
                resource->val.cval = (char *)
                    XtMalloc(strlen(savedResourceString) + 1);
                strcpy(resource->val.cval, savedResourceString);
                XtFree(savedResourceString);
                objectExpanded = False;
            }
        } else if (!strcmp(resource->name, "interactive")) {
            if (resource->variable)
                ExpandVariable(resource, (char *) ptr, type);
            if (resource->val.cval == NULL) {
                return;
            }
            if (strrchr(resource->val.cval, '[')) {
                savedResourceString = XtMalloc(strlen(resource->val.cval) + 1);
                strcpy(savedResourceString, resource->val.cval);
                ExpandObjectValues(resource, ptr, type);
                objectExpanded = True;
            }
            bzero((char *) command, sizeof(Command));
            command->activeshell = (char *) XtMalloc(strlen(resource->val.cval) + 1);
            command->path = (char *) XtMalloc(strlen(resource->val.cval) + 1);
            bzero(command->path, sizeof(command->path));
            strcpy(command->activeshell, resource->val.cval);
            GlobalActiveShell.interactiveshell = True;
            DoExec(command);
            if (objectExpanded) {
                XtFree(resource->val.cval);
                resource->val.cval = (char *)
                    XtMalloc(strlen(savedResourceString) + 1);
                strcpy(resource->val.cval, savedResourceString);
                XtFree(savedResourceString);
                objectExpanded = False;
            }
        } else if (!strcmp(resource->name, "runbackground")) {
            /* set command arglist to include the result of expanding */
            if (resource == inresource) {
                if (resource->variable)
                    ExpandVariable(resource, (char *) ptr, type);
                if (resource->val.cval == NULL) {
                    fprintf(stderr, "\tignoring command\n\n");
                    return;
                }
                if (strrchr(resource->val.cval, '[')) {
                    savedResourceString = XtMalloc(strlen(resource->val.cval) + 1);
                    strcpy(savedResourceString, resource->val.cval);
                    ExpandObjectValues(resource, ptr, type);
                    objectExpanded = True;
                }
                command->path = (char *) XtMalloc(strlen(resource->val.cval) + 1);
                strcpy(command->path, resource->val.cval);
                command->dowait = False;
                DoExec(command);
                if (objectExpanded) {
                    XtFree(resource->val.cval);
                    resource->val.cval = (char *)
                        XtMalloc(strlen(savedResourceString) + 1);
                    strcpy(resource->val.cval, savedResourceString);
                    XtFree(savedResourceString);
                    objectExpanded = False;
                }
                return;
            } else
                bzero((char *) command, sizeof(Command));
        } else if (!strcmp(resource->name, "postnotice")) {
	    DoNotice((char *) ptr, inresource, type);
        } else if (!strcmp(resource->name, "runforeground")) {
            /* set command arglist to include the result of expanding */
            if (resource == inresource) {
                if (resource->variable)
                    ExpandVariable(resource, (char *) ptr, type);
                if (resource->val.cval == NULL) {
                    fprintf(stderr, "\tignoring command\n\n");
                    return;
                }
                if (strrchr(resource->val.cval, '[')) {
                    savedResourceString = XtMalloc(strlen(resource->val.cval) + 1);
                    strcpy(savedResourceString, resource->val.cval);
                    ExpandObjectValues(resource, ptr, type);
                    objectExpanded = True;
                }
                command->path = (char *) XtMalloc(strlen(resource->val.cval) + 1);
                strcpy(command->path, resource->val.cval);
                command->dowait = True;
                DoExec(command);
                if (objectExpanded) {
                    XtFree(resource->val.cval);
                    resource->val.cval = (char *)
                        XtMalloc(strlen(savedResourceString) + 1);
                    strcpy(resource->val.cval, savedResourceString);
                    XtFree(savedResourceString);
                    objectExpanded = False;
                }
                return;
            } else
                bzero((char *) command, sizeof(Command));
        }
        resource = resource->next;
    }
}
