/**********************************************************************
   expandvar.c  - perform variable expansion
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
ExpandVariable(resource, ptr, ptrType)
    Resource                       *resource;
    caddr_t                         ptr;
    int                             ptrType;
{
    char                           *savePtr = resource->varValue;
    char                           *tmp, *getenv();
    Boolean                         save = verbose;

    if (resource->type == STRING && resource->variable) {
        char                            expanded[8192];

        if (resource->varValue == NULL) {
	    verbose = True;
            sprintf(errorbuf,
                    "initial resource value is null %s",
                    resource->val.cval);
            XgenWarning("expand variables", errorbuf);
            resource->val.cval = NULL;
            ExpandError(resource, ptr, ptrType);
            XgenExit(-1);
        }
        strcpy(expanded, resource->varValue);
        switch (ExpandString(expanded, 8192)) {
        case -1:
	    verbose = True;
            sprintf(errorbuf,
                    "a variable in string [%s] is undefined ",
                    resource->varValue);
            XgenWarning("expand variables", errorbuf);
            ExpandError(resource, ptr, ptrType);
            XgenExit(-1);
        case 0:
            if (resource->val.cval)
                XtFree(resource->val.cval);
            resource->val.cval = SaveString(expanded);
            break;
        case 1:
            sprintf(errorbuf, "string [%s] has been truncated, and may cause unpredictable results\n",
                    resource->varValue);
	    verbose = True;
            XgenWarning("expand variables", errorbuf);
            ExpandError(resource, ptr, ptrType);
	    verbose = save;
            return;
        }
    }
    if (*savePtr == '$') {
        savePtr++;
        tmp = getenv(savePtr);
        if (NULL == tmp) {
            sprintf(errorbuf, "string [%s] has been truncated\n",
                    resource->val.cval);
	    verbose = True;
            XgenWarning("expand variables", errorbuf);
            ExpandError(resource, ptr, ptrType);
	    verbose = save;
            return;
        }
        switch (resource->type) {
        case INTEGER:
            if (CheckType(tmp, Int))
                resource->val.ival = atoi(tmp);
            else
                XgenFatalError("expand variable", errorbuf);
            break;
        case REAL:
            if (CheckType(tmp, Real))
#ifdef SVR4
                resource->val.dval = strtod(tmp, NULL);
#else
#ifdef BSD
                resource->val.dval = atof(tmp);
#else
                resource->val.dval = strtod(tmp, NULL);
#endif
#endif
            else
                XgenFatalError("expand variable", errorbuf);
            break;
        case BOOLEAN:
            if (CheckType(tmp, OnOff))
                resource->val.bval =
                    (((!strcmp(tmp, "True")) ||
                      (!strcmp(tmp, "true")) ||
                      (!strcmp(tmp, "On")) ||
                      (!strcmp(tmp, "on"))) ? True : False);
            else
                XgenFatalError("expand variable", errorbuf);
            break;
        }
    }
    return;
}
