/**********************************************************************
   resource.c   - operate on resource data structures
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
#include "y.tab.h"
#include "resource.h"
#define END(v) (v-1 + sizeof v / sizeof v[0])


char                           *
ResourceString(resource)
    int                             resource;
{
    struct _resourceTable          *low = resourceTable, *high = END(resourceTable);

    while (low <= high) {
        if (low->index == resource)
            return low->name;
        low++;
    }
    return (NULL);
}

int
ResourceIndex(resource)
    char                           *resource;
{
    struct _resourceTable          *low = resourceTable, *high = END(resourceTable);

    while (low <= high) {
        if (!strcmp(low->name, resource))
            return low->index;
        low++;
    }
    return (*(int *) NULL);
}

int
ResourceDataType(resource)
    int                             resource;
{
    struct _resourceTable          *low = resourceTable, *high = END(resourceTable);

    while (low <= high) {
        if (low->index == resource)
            return low->type;
        low++;
    }
    return (UNKNOWN);
}

unsigned int
ResourceValid(resource)
    int                             resource;
{
    struct _resourceTable          *low = resourceTable, *high = END(resourceTable);

    while (low <= high) {
        if (low->index == resource)
            return low->valid;
        low++;
    }
    return (1L << 0);
}

unsigned int
ShellObjectValid(shell)
    int                             shell;
{
    struct _shellTable             *low = shellTable, *high = END(shellTable);

    while (low <= high) {
        if (low->index == shell)
            return low->valid;
        low++;
    }
    return (1L << 0);
}

char                           *
ShellString(shell)
    int                             shell;
{
    struct _shellTable             *low = shellTable, *high = END(shellTable);

    while (low <= high) {
        if (low->index == shell)
            return low->name;
        low++;
    }
    return (NULL);
}

char                           *
ObjectString(object)
    int                             object;
{
    struct _objectTable            *low = objectTable, *high = END(objectTable);

    while (low <= high) {
        if (low->index == object)
            return low->name;
        low++;
    }
    return (NULL);
}

Resource                       *
IndexResource(cptr, type, rname)
    caddr_t                         cptr;
    int                             type;
    char                           *rname;
{
    Resource                       *p;

    switch (type) {
    case ENVIRONMENT:
        {
            Environ                        *ptr = (Environ *) cptr;

            for (p = ptr->resources; p != NULL; p = p->next)
                if (!strcmp(p->name, rname))
                    return p;
        }
        break;
    case SHELL:
        {
            Shell                          *ptr = (Shell *) cptr;

            for (p = ptr->resources; p != NULL; p = p->next)
                if (!strcmp(p->name, rname))
                    return p;
        }
        break;
    case OBJECT:
        {
            InterfaceObject                *ptr = (InterfaceObject *) cptr;

            for (p = ptr->resources; p != NULL; p = p->next)
                if (!strcmp(p->name, rname))
                    return p;
        }
        break;
    }
    return NULL;
}
