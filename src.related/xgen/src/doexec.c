/**********************************************************************
   doexec.c     - execute a shell command
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

/* 
 * exec a command given in a Command structure.
 */

void
DoExec(com)
    Command *com;
{
    char command[1024];
    int actual_kid;
    int child;
    union wait status;
    void ReapChild();
    char *template;
    char *mktemp();
        Pixmap hourglassPixmap;
        Pixmap hourglassmaskPixmap;
        Cursor hourglassCursor;
        XColor blackColor, whiteColor, exact;
        Status resBlack, resWhite;

    /* define the hourglass cursor for all windows */
        /* get the colors for the cursor */
        resBlack = XAllocNamedColor(xgenGD.display,xgenGD.cmap,"black",
                                    &exact,&blackColor);
        resWhite = XAllocNamedColor(xgenGD.display,xgenGD.cmap,"white",
                                    &exact,&whiteColor);
        if ( com->dowait && resBlack && resWhite  ) {
            Shell *s;
        /* create the pixmap and mask */
            hourglassPixmap =
                XCreateBitmapFromData(xgenGD.display,XtWindow(xgenGD.applShell),
                   hourglass_bits,hourglass_width,hourglass_height);
            hourglassmaskPixmap =
                XCreateBitmapFromData(xgenGD.display,XtWindow(xgenGD.applShell),
                   hourglassmask_bits,hourglassmask_width,hourglassmask_height);
            hourglassCursor = 
                XCreatePixmapCursor(xgenGD.display,hourglassPixmap,
                   hourglassmaskPixmap,&blackColor,&whiteColor,
                   hourglass_x_hot, hourglass_y_hot);
            s = xgenGD.currentEnv->shells;
            while ( s ) {
                if ( s->popup )
                    XDefineCursor(xgenGD.display,XtWindow(s->widget),
                          hourglassCursor);
                s = s->next;
            }
            /* 
             * flush the cursor requests to the display before the command
             * starts us into a wait state...
             */
            XFlush(xgenGD.display);
        }
    /* construct the command */

    sprintf(command,"%s",com->path);
    if ( com->arglist )
        sprintf(command,"%s %s",command,com->arglist);

    /* check for NULL command. */

    if ( !strcmp(command,"") ) return;

    /**************************************************************
     * Here is some code that will catch users passing foreground 
     * commands with &'s.
     * If you need to keep control over these children.
     * If you use this code some background commands will keep things 
     * bogged down, and nothing will happen anyway.
     **************************************************************/
    /* KAB: see comment above 
    if (  NULL != (ptr = rindex(command,'&')) ) {
        Boolean gotcha = False;

        save = ptr;
        if ( *(ptr + 1) == '\0' ) {
            *save = ' ';
            com->dowait = False;
        } else {
            ptr++;
            gotcha = True;
            while( *ptr != '\0' )  {
                if ( !isspace(*ptr) )  {
                    gotcha = False;
                }
                ptr++;
            }
            if ( gotcha ) {
                *save = ' ';
                com->dowait = False;
            }
        }
        if ( *(ptr - 1) != '>' ) {
            *save = ' ';
        }
    }
     KAB: see comment above */
    /***************************************************************
      * Set up SIGCHLD handler to deal with background children as they die
     **************************************************************/
    if (!com->dowait) (void) signal(SIGCHLD,ReapChild);
    /***************************************************************
      * If we are capturing output in a scrolltext window get a 
     * temporary filename.
     **************************************************************/
    if ( com->capture ) 
        if ( !strcmp(com->sink,"null") ) {
            /* get filename given by mktemp */
            template = XtMalloc(10);
            sprintf(template,"/tmp/XgSXXXXXX");
            com->tmpfile = mktemp(template);
        }
    /***************************************************************
      * Get a temporary filename for the error output.
     **************************************************************/
    template = XtMalloc(10);
    sprintf(template,"/tmp/XgEXXXXXX");
    com->errfile = mktemp(template);
    
    /***************************************************************
      * Flush out any buffered data.
     **************************************************************/
    fflush(stdout);
    fflush(stderr);

    /***************************************************************
      * Fork.
     **************************************************************/
    switch (child = fork()) {
        case -1: /* Error */
            perror("fork");
        case 0: /* The Child */

            /* if we're doing input dup stdin */
            if ( com->input ) 
                DoDup(com->source,0);

            /* if we're capturing output */
            if ( com->capture ) {
                /* in a scroll text area */
                if ( !strcmp(com->sink,"null") ) 
                    DoDup(com->tmpfile,1);
                /* in a file */
                else
                    DoDup(com->sink,1);
            }
            DoDup(com->errfile,2);

            execl("/bin/sh","sh","-c",command,0);
            _exit(127);
        default: /* The parent */
            actual_kid = child;
            break;
    }
    /***************************************************************
     * if this is a foreground process, wait for it
     **************************************************************/
    if (com->dowait) {
        int w;

        while ( (w = wait (&status)) != actual_kid && w != -1)
                ;
        /***************************************************************
         * if we are to capture output in a scroll text space
        * then unlink the temporary file and 
        * free the space allocated for the name
         **************************************************************/
        if ( com->capture && !strcmp(com->sink,"null")) {
            DoCaptureText(com);
            unlink(com->tmpfile);
            XtFree(com->tmpfile);
        }
        /***************************************************************
         * if there was an error, popup error dialog...
        * then unlink the temporary file and 
        * free the space allocated for the name
         **************************************************************/
        if ( WIFEXITED(status) && status.w_retcode != 0) 
            DoError(com);
        unlink(com->errfile);
        XtFree(com->errfile);
                /* if we were successful with our cursor */
                if ( resBlack && resWhite  ) {
                    Shell *s = xgenGD.currentEnv->shells;
                    while ( s ) {
                        if ( s->popup )
                            XUndefineCursor(xgenGD.display,XtWindow(s->widget));
                        s = s->next;
                    }
                    XFreeCursor(xgenGD.display,hourglassCursor);
                }
        fflush(stdout);
    } else {
    /***************************************************************
     * update the pid field and stash the structure away.
     **************************************************************/
        com->pid = actual_kid /*+ 1*/; 
        setpgrp(com->pid,com->pid);
        AddCommand(com);
        if ( !nocontrol )
            AddCommandToControlBox(com);
        XFlush(xgenGD.display);
        fflush(stdout);
    }
}

void
ReapChild()
{
    int pid;
    union wait status;

    /***************************************************************
     * if nothing is wrong
     **************************************************************/
    if((pid = wait3(&status, WNOHANG, (struct rusage*)0)) > 0) 
        /***************************************************************
         * if nothing is wrong
         **************************************************************/
        DoReap(status,pid);
}

DoReap(status,pid)
    union wait status;
    int pid;
{
    /***************************************************************
     * if the process exited with a non-zero return code, sh detected
     * an error. Display the contents of the error file in the error
     * dialog.
     **************************************************************/
        if ( WIFEXITED(status) && status.w_retcode != 0) {
            Command *command;

            if ( NULL != (command = FindCommand(pid)) ) 
                DoError(command);
        }
    /***************************************************************
     * delete the command from the list.
     **************************************************************/
        if ( !WIFSIGNALED(status) ) {
            DeleteCommandFromControlBox(pid);
            DeleteCommand(pid);
        }
}

