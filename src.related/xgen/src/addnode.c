/**********************************************************************
   addnode.c    - add a node to Xgen's internal hierarchy

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
 * AddValue
 * Add a value node to a resource node. Take into account
 * the type of value and assign to the proper union element.
 * If the resource is a variable, store it for later expansion.
 * Unless it is a run or set procedure.
 **************************************************************/
void
AddValue(rptr, value, rtype)
    Resource                       *rptr;
    char                           *value;
    int                             rtype;
{
    rptr->name = ResourceString(rtype);
    rptr->type = ResourceDataType(rtype);
    if (rptr->type != STRING && IsVariable(value)) {
        rptr->variable = True;
        rptr->varValue = SaveString(value);
    } else {
        switch (rptr->type) {
        case STRING:
            /***************************************************************
             * if the string has a variable and its not a set command
             * or related in any way to running jobs, assign the value to
             * the variable field, else assign the value straight to
             * the resource.
             **************************************************************/
            if (rptr->variable && (strncmp(rptr->name, "set", 3) ||
                                   strncmp(rptr->name, "run", 3) ||
                                   strcmp(rptr->name, "commandarg")))
                rptr->varValue = value;
            else
                rptr->val.cval = value;
            break;
        case REAL:
#ifdef SVR4
            rptr->val.dval = strtod(value, (char **) NULL);
#else
#ifdef BSD
            rptr->val.dval = atof(value);
#else
            rptr->val.dval = strtod(value, (char **) NULL);
#endif
#endif
            break;
        case INTEGER:
            rptr->val.ival = atoi(value);
            break;
        case BOOLEAN:
            rptr->val.bval =
                (((!strcmp(value, "True")) ||
                  (!strcmp(value, "true")) ||
                  (!strcmp(value, "On")) ||
                  (!strcmp(value, "on"))) ? True : False);
            break;
        }
    }
}

/***************************************************************
 * AddResource
 * Add a value resource to a node. This function will add a
 * resource node to any node type.
 **************************************************************/
void
AddResource(rptr, cptr, type, value, rtype)
    Resource                       *rptr;
    caddr_t                         cptr;
    int                             type;
    char                           *value;
    int                             rtype;
{
    Resource                       *p;

    switch (type) {
    case ENVIRONMENT:
        {
            Environ                        *ptr = (Environ *) cptr;

            if (ptr->resources == (Resource *) 0) {
                ptr->resources = rptr;
                AddValue(ptr->resources, value, rtype);
                return;
            }
            p = ptr->resources;
            while (p->next != (Resource *) 0)
                p = p->next;
            p->next = rptr;
            AddValue(p->next, value, rtype);
        }
        break;
    case SHELL:
        {
            Shell                          *ptr = (Shell *) cptr;

            if (ptr->resources == (Resource *) 0) {
                ptr->resources = rptr;
                AddValue(ptr->resources, value, rtype);
                return;
            }
            p = ptr->resources;
            while (p->next != (Resource *) 0)
                p = p->next;
            p->next = rptr;
            AddValue(p->next, value, rtype);
        }
        break;
    case OBJECT:
        {
            InterfaceObject                *ptr = (InterfaceObject *) cptr;

            if (ptr->resources == (Resource *) 0) {
                ptr->resources = rptr;
                AddValue(ptr->resources, value, rtype);
                return;
            }
            p = ptr->resources;
            while (p->next != (Resource *) 0)
                p = p->next;
            p->next = rptr;
            AddValue(p->next, value, rtype);
        }
        break;
    default:
        XgenFatalError("AddResource", "invalid resource type");
    }
}

/***************************************************************
 * AddObject
 * Add an object node to a shell node.
 **************************************************************/
void
AddObject(optr, sptr)
    InterfaceObject                *optr;
    Shell                          *sptr;
{
    InterfaceObject                *p;

    if (sptr->objects == (InterfaceObject *) 0) {
        sptr->objects = optr;
        return;
    }
    p = sptr->objects;
    while (p->next != (InterfaceObject *) 0)
        p = p->next;
    p->next = optr;
}

/***************************************************************
 * AddPulldownObject
 * Add an object node to a pulldown object node.
 **************************************************************/
void
AddPulldownObject(optr, pptr)
    InterfaceObject                *optr;
    InterfaceObject                *pptr;
{
    InterfaceObject                *p;

    if (pptr->objects == (InterfaceObject *) 0) {
        pptr->objects = optr;
        return;
    }
    p = pptr->objects;
    while (p->next != (InterfaceObject *) 0)
        p = p->next;
    p->next = optr;
}

/***************************************************************
 * AddShell
 * Add an shell node to a environment node.
 **************************************************************/
void
AddShell(sptr, eptr)
    Shell                          *sptr;
    Environ                        *eptr;
{
    Shell                          *p;

    if (eptr->shells == (Shell *) 0) {
        eptr->shells = sptr;
        return;
    }
    p = eptr->shells;
    while (p->next != (Shell *) 0)
        p = p->next;
    p->next = sptr;
}

/***************************************************************
 * AddEnviron
 * Add an environment node to the global list.
 **************************************************************/
void
AddEnviron(eptr)
    Environ                        *eptr;
{
    Environ                        *p;

    if (xgenGD.toplevelEnv == (Environ *) 0) {
        xgenGD.toplevelEnv = eptr;
        return;
    }
    p = xgenGD.toplevelEnv;
    while (p->next != (Environ *) 0)
        p = p->next;
    p->next = eptr;
}
