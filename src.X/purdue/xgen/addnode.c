/***************************************************************
 * addnode.c 
 *
 * This file contains functions for adding new nodes to Xgen's
 * internal hierarchy.
 * Node types include environments, shell, objects, resources, 
 * and resource values
 **************************************************************/

#include "xgen.h"

/***************************************************************
 * AddValue 
 * Add a value node to a resource node. Take into account
 * the type of value and assign to the proper union element.
 * If the resource is a variable, store it for later expansion.
 * Unless it is a run or set procedure.
 **************************************************************/
AddValue(rptr,value,rtype)
    Resource *rptr;
    char *value;
    int rtype;
{
    rptr->name = ResourceString(rtype);
    rptr->type = ResourceDataType(rtype);
    if ( rptr->type != STRING && IsVariable(value)) {
        rptr->variable = True;
        rptr->varValue = value;
    } else {
        switch(rptr->type) {
            case STRING:
               /***************************************************************
                * if the string has a variable and its not a set command
                * or related in any way to running jobs, assign the value to
                * the variable field, else assign the value straight to
                * the resource.
                **************************************************************/
                if ( rptr->variable && strncmp(rptr->name,"set",3) &&
                     strncmp(rptr->name,"run",3) && 
                     strcmp(rptr->name,"commandarg")) 
                    rptr->varValue = value;
                else
                    rptr->val.cval = value;
                break;
            case REAL:
                rptr->val.dval = strtod(value);
                break;
            case INTEGER:
                rptr->val.ival = atoi(value);
                break;
            case BOOLEAN:
                rptr->val.bval = 
                (((!strcmp(value,"True")) ||
                  (!strcmp(value,"true")) ||
                  (!strcmp(value,"On"))   ||
                  (!strcmp(value,"on")))  ? True:False);
                break;
        }
    }
}

/***************************************************************
 * AddResource 
 * Add a value resource to a node. This function will add a 
 * resource node to any node type.
 **************************************************************/
AddResource(rptr,cptr,type,value,rtype) 
    Resource *rptr;
    caddr_t cptr;
    int type;
    char *value;
    int rtype;
{
    Resource *p;

    switch(type) {
        case ENVIRONMENT:
            {
            Environ *ptr = (Environ *)cptr;

            if ( ptr->resources == (Resource *)0 ) {
                ptr->resources = rptr;
                AddValue(ptr->resources,value,rtype);
                return;
            }
            for ( p = ptr->resources; p->next != (Resource *)0; p = p->next);
            p->next = rptr;
            AddValue(p->next,value,rtype);
            }
            break;
        case SHELL:
            {
            Shell *ptr = (Shell *)cptr;

            if ( ptr->resources == (Resource *)0 ) {
                ptr->resources = rptr;
                AddValue(ptr->resources,value,rtype);
                return;
            }
            for ( p = ptr->resources; p->next != (Resource *)0; p = p->next);
            p->next = rptr;
            AddValue(p->next,value,rtype);
            }
            break;
        case OBJECT:
            {
            InterfaceObject *ptr = (InterfaceObject *)cptr;

            if ( ptr->resources == (Resource *)0 ) {
                ptr->resources = rptr;
                AddValue(ptr->resources,value,rtype);
                return;
            }
            for ( p = ptr->resources; p->next != (Resource *)0; p = p->next);
            p->next = rptr;
            AddValue(p->next,value,rtype);
            }
            break;
        default:
            XgenFatalError("AddResource","invalid resource type");
    }
}

/***************************************************************
 * AddObject 
 * Add an object node to a shell node.
 **************************************************************/
AddObject(optr,sptr) 
    InterfaceObject *optr;
    Shell *sptr;
{
    InterfaceObject *p;

    if ( sptr->objects == (InterfaceObject *)0 ) {
        sptr->objects = optr;
        return;
    }
    for ( p = sptr->objects; p->next != (InterfaceObject *)0; p = p->next ) ;
    p->next = optr;
}

/***************************************************************
 * AddPulldownObject 
 * Add an object node to a pulldown object node.
 **************************************************************/
AddPulldownObject(optr,pptr) 
    InterfaceObject *optr;
    InterfaceObject *pptr;
{
    InterfaceObject *p;

    if ( pptr->objects == (InterfaceObject *)0 ) {
        pptr->objects = optr;
        return;
    }
    for ( p = pptr->objects; p->next != (InterfaceObject *)0; p = p->next ) ;
    p->next = optr;
}

/***************************************************************
 * AddShell 
 * Add an shell node to a environment node.
 **************************************************************/
AddShell(sptr,eptr)
    Shell *sptr;
    Environ *eptr;
{
    Shell *p;

    if ( eptr->shells == (Shell *)0 ) {
        eptr->shells = sptr;
        return;
    }
    for ( p = eptr->shells; p->next != (Shell *)0; p = p->next ) ;
    p->next = sptr;
}

/***************************************************************
 * AddEnviron 
 * Add an environment node to the global list.
 **************************************************************/
AddEnviron(eptr) 
    Environ *eptr;
{
    Environ *p; 

    if ( xgenGD.toplevelEnv == (Environ *)0 ) {
        xgenGD.toplevelEnv = eptr;
        return;
    }
    for ( p = xgenGD.toplevelEnv; p->next != (Environ *)0; p = p->next ) ;
    p->next = eptr;
}
