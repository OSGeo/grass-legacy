/**********************************************************************
   buttonname.c - replace the buttonname keyword with text
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
 * ReplaceButtonName - replace everything after = in s with name
 **************************************************************/
void
ReplaceButtonName(s, name, space)
    char                           *s;
    char                           *name;
    Boolean                         space;
{
    char                           *ptr;

    /* set ptr to the '=' in s */
    ptr = (char *) strrchr(s, '=');
    /* increment past the '=' */
    ptr++;
    /****************************************************************
     * while not at the end of the string increment past
     * leading whitespace in name .
     ****************************************************************/
    while (*name && isspace(*name))
        name++;
    /****************************************************************
     * while not at the end of the string
     ****************************************************************/
    while (*name) {
        /* and this isn't a newline */
        if (*name != '\n')
            /* replace the first element with that in name */
            *ptr++ = *name++;
        /* this IS a newline, replace it with a space (or not) */
        else {
            name++;
            if (space)
                *ptr++ = ' ';
        }
    }
    /* null terminate */
    *ptr = '\0';
}
