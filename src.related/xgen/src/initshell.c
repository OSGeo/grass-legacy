/**********************************************************************
   initshell.c  - perform initialshell command action
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

void
InitialShell(eptr)
    Environ                        *eptr;
{
    /* try to retrieve the initialshells resource */
    Resource                       *ishells = IndexResource((char *) eptr, ENVIRONMENT, "initialshells");
    char                           *ilist = NULL;
    char                           *token;
    Boolean                         first = False;

    /* if couldn't find them set the initialshells to the first shell */
    if (ishells == NULL) {
        ilist = eptr->shells->name;
        AddResource(AllocResource(), (char *) eptr, ENVIRONMENT, ilist, InitialShells);
    } else
        ilist = ishells->val.cval;

    /* parse the lists (if more than one name occurs) and turn on flags */
    /* initial shells */
    do {
        if (!first) {
            token = (char *) strtok(ilist, " ");
            first = True;
        } else
            token = (char *) strtok(NULL, " ");
        if (token) {
            Shell                          *s;

            if ((s = IndexShell(token)) == NULL) {

                sprintf(errorbuf, "non-existant shell %s", token);
                XgenFatalError("processing initial shells", errorbuf);
            }
            s->initial = True;
        }
    } while (token != NULL);
    ilist = NULL;

}
