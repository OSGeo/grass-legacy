/**********************************************************************
   command.c    - operate on the command structure
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
AddCommand(command)
    Command                        *command;
{
    Command                        *tmp = xgenGD.commandList;

    if (tmp == NULL) {
        xgenGD.commandList = command;
        return;
    }
    while (tmp->next != NULL)
        tmp = tmp->next;
    tmp->next = command;
    return;
}

void
DeleteCommand(pid)
    PIDTYPE                         pid;
{
    /***************************************************************
     * Find the command in the command list and update the list.
     **************************************************************/
    Command                        *goner = FindCommand(pid);
    Command                        *tmp;

    if (goner == NULL)
        return;
    if (goner == xgenGD.commandList)
        xgenGD.commandList = goner->next;
    else {
        tmp = xgenGD.commandList;
        while ((tmp != NULL) && (tmp->next != goner))
            tmp = tmp->next;
        if (tmp == NULL)
            return;
        tmp->next = tmp->next->next;
    }
    /***************************************************************
     * remove temporary files and free up space allocated in DoExec
     **************************************************************/
    if (goner->capture && !strcmp(goner->sink, "null")) {
        unlink(goner->tmpfile);
        XtFree(goner->tmpfile);
    }
    unlink(goner->errfile);
    XtFree(goner->errfile);
    XtFree(goner->buf);
    /***************************************************************
     * Free up the rest
     **************************************************************/
    XtFree((char *) goner);
}

Command                        *
FindCommand(pid)
    PIDTYPE                         pid;
{
    Command                        *tmp;

    for (tmp = xgenGD.commandList; tmp != NULL; tmp = tmp->next)
        if (tmp->pid == pid)
            return tmp;
    return NULL;
}
