/**********************************************************************
   DoInteract.c - run and deal with an interactive command
 **********************************************************************/
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
/*******
 * Code for interactive commands donated by:
 *           George Dolbier sysadmin at large
 *      georged@sequent.com or gdolbier@us.oracle.com
 *******/
#include "xgen.h"

void
StartInteractive(com)
    Command                        *com;
{
    PIDTYPE                         pid;
    char                           *mktemp();


    /***************************************************************
      * Flush out any buffered data.
     **************************************************************/

    fflush(stdout);
    fflush(stderr);
    /***************************************************************
      * start interactive shell, thanks bills@sequent.com!!
     **************************************************************/
    if (pipe(GlobalActiveShell.filedes) < 0) {
        perror("Can't create pipe");
        return;
    }
    if ((pid = fork()) < 0) {
        perror("Can't fork");
        return;
    } else if (pid == 0) {
        close(GlobalActiveShell.filedes[1]);
        dup2(GlobalActiveShell.filedes[0], 0);
        execl("/bin/sh", "sh", "-c", com->activeshell, 0);
        perror("Can't execl");
        _exit(1);
    }
    GlobalActiveShell.activeshellrunning = True;
}

void
InteractiveCommand(com)
    Command                        *com;
{
    char                            command[1024];
    char                           *mktemp();
    char                           *template;

    /***************************************************************
       the string "KillInterActive" will attempt to stop the
       interactive shell by closing it's input
    **************************************************************/

    if (!strcmp(com->path, "KillInterActive")) {
        close(GlobalActiveShell.filedes[0]);
        close(GlobalActiveShell.filedes[1]);
        GlobalActiveShell.activeshellrunning = False;
        GlobalActiveShell.interactiveshell = False;
        return;
    }
    /***************************************************************
         set up file name for error outpup
    **************************************************************/

    template = XtMalloc(strlen("/tmp/XgenErrXXXXXX") + 1);
    strcpy(template, "/tmp/XgenErrXXXXXX");
    com->errfile = mktemp(template);
    DoDup(com->errfile, 2);

    strcpy(command, com->path);
    strcat(command, "\n");
    /*
     * If you want to block a blank command here is how if (
     * !strcmp(command,"\n") ) { fprintf(stderr,"Warning: running a null
     * command"); fprintf(stderr," may cause un-perdictable result\n"); }
     */

    /***************************************************************
    feed the command to the waiting program
    **************************************************************/
    close(GlobalActiveShell.filedes[0]);
    write(GlobalActiveShell.filedes[1], command, strlen(command));


    /***************************************************************
      if there was an error inform the user
     ************************************************************** */
    DoError(com);
    unlink(com->errfile);
    XtFree(com->errfile);

}
