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
#define DOEXEC
#include "xgen.h"
#undef DOEXEC

XtTimerCallbackProc
#ifdef _NO_PROTO
DoFinishCommand(client_data, id)
    XtPointer client_data; XtIntervalId *id;
#else
DoFinishCommand(XtPointer client_data, XtIntervalId *id)
#endif
{
    if (xgenGD.commandList) {
	PIDTYPE                         pid;
	Command                        *ptr;
#ifdef SVR4
	int                             status;
#else
#ifdef BSD
	union wait                      status;
#else
	int                             status;
#endif
#endif
	int                             deletepid = 0;

	ptr = xgenGD.commandList;
	while (ptr && !deletepid) {
#ifdef SVR4
	    if ((pid = waitpid(ptr->pid, &status, WNOHANG)) == ptr->pid) {
#else
#ifdef BSD
	    if ((pid = wait3(&status, WNOHANG, NULL)) == ptr->pid) {
#else
	    if ((pid = waitpid(ptr->pid, &status, WNOHANG)) == ptr->pid) {
#endif
#endif

		/* Check for errors */
		if (WIFEXITED(status) != 0) {
		    Boolean                         acceptable = False;

		    /*
		     * for  ( i = 0; i < ptr->nerr; i++ ) { if (
		     * WEXITSTATUS(status) == ptr->err[i] ) { acceptable
		     * = True; } }
		     */
#ifdef SVR4
		    if (WEXITSTATUS(status) == 0) {
#else
#ifdef BSD
		    if (status.w_retcode == 0) {
#else
		    if (WEXITSTATUS(status) == 0) {
#endif
#endif
			acceptable = True;
		    }
		    if (!acceptable) {
			DoError(ptr);
		    } else {
			if (ptr->capture && !strcmp(ptr->sink, "editor")) {
			    DoCapture(ptr);
			}
		    }
		    deletepid = pid;

		}
	    }
	    if (!deletepid)
		ptr = ptr->next;
	}
        if ( ptr && ptr->notify ) {
	    if (ptr->arglist) {
		sprintf(errorbuf, "Background Job Complete:\n%s %s",
                    ptr->path,ptr->arglist);
	    } else {
		sprintf(errorbuf, "Background Job Complete:\n%s",ptr->path);
	    }

            XgNotifyDialog(ptr->shell,errorbuf);
        }
	if (ptr && deletepid) {
	    unlink(ptr->errfile);
	    if (ptr->capture && !strcmp(ptr->sink, "editor"))
		unlink(ptr->tmpfile);
	    DeleteCommandFromControlBox(deletepid);
	    DeleteCommand(deletepid);
	}
    }
    return (NULL);
}
/*
 * exec a command given in a Command structure.
 */


void
DoExec(com)
    Command                        *com;
{
    char                            command[1024];
    int                             actual_kid;
    int                             child;
#ifdef SVR4
    int                             status;
#else
#ifdef BSD
    union wait                      status;
#else
    int                             status;
#endif
#endif

#ifdef BSD
    int KidDied();
#else
    void KidDied();
#endif
    char                           *tmpnam();
    char                           *ampersand;
    Pixmap                          hourglassPixmap;
    Pixmap                          hourglassmaskPixmap;
    Cursor                          hourglassCursor;
    XColor                          blackColor, whiteColor, exact;
    Status                          resBlack, resWhite;

    if (GlobalActiveShell.interactiveshell == True) {
        if (GlobalActiveShell.activeshellrunning != True) {
            StartInteractive(com);
            return;
        } else {
            InteractiveCommand(com);
            return;
        }
    }
    /* define the hourglass cursor for all windows */
    /* get the colors for the cursor */
    resBlack = XAllocNamedColor(xgenGD.display, xgenGD.cmap, "black",
                                &exact, &blackColor);
    resWhite = XAllocNamedColor(xgenGD.display, xgenGD.cmap, "white",
                                &exact, &whiteColor);
    if (com->dowait && resBlack && resWhite) {
        Shell                          *s;
        /* create the pixmap and mask */
        hourglassPixmap =
            XCreateBitmapFromData(xgenGD.display, XtWindow(xgenGD.applShell),
                (char *) hourglass_bits, hourglass_width, hourglass_height);
        hourglassmaskPixmap =
            XCreateBitmapFromData(xgenGD.display, XtWindow(xgenGD.applShell),
                           (char *) hourglassmask_bits, hourglassmask_width,
                                  hourglassmask_height);
        hourglassCursor =
            XCreatePixmapCursor(xgenGD.display, hourglassPixmap,
                              hourglassmaskPixmap, &blackColor, &whiteColor,
                                hourglass_x_hot, hourglass_y_hot);
        s = xgenGD.currentEnv->shells;
        while (s) {
            if (s->popup)
                XDefineCursor(xgenGD.display, XtWindow(s->widget),
                              hourglassCursor);
            s = s->next;
        }
        /*
         * flush the cursor requests to the display before the command starts
         * us into a wait state...
         */
        XFlush(xgenGD.display);
    }

    /* construct the command */
    bzero(command, sizeof(command));
    strcpy(command, com->path);
    if (com->arglist) {
        strcat(command, com->arglist);
    }
    /*
     * check for NULL command. if null make shure that it's null had problems
     * with a null command being filled with bleech
     */

    if (!strcmp(command, "")) {
        fprintf(stderr,
        "Warning: running a null command may cause un-perdictable result\n");
        com->buf = NULL;
    } else {
        com->buf = XtNewString(command);
    }
    /***************************************************************
      * If we are capturing output in a scrolltext window get a
     * temporary filename.
     **************************************************************/
    if (com->capture) {
        if (!strcmp(com->sink, "editor")) {
            /* get filename given by mktemp */
            com->tmpfile = XtNewString(tmpnam(NULL));
        }
        if (!strcmp(com->sink, "append")) {
            /*
             * if the output is append and the file has not been setup
             * yet, set the file up, else point the command tmpfile at the
             * global tmpfile and go on
             */
            if (!xgenGD.KitchenOpen) {
                xgenGD.KitchenSink = tmpnam(NULL);
                xgenGD.KitchenOpen = False;
            } else {
                com->tmpfile = xgenGD.KitchenSink;
                xgenGD.KitchenOpen = True;
            }
        }
        if (!strcmp(com->sink, "continuumOFF")) {
            /*
             * if we turn off we want to unlink the file and turn
             * com->capture off
             */
            xgenGD.KitchenOpen = False;
            com->capture = False;
            unlink(xgenGD.KitchenSink);
            XtFree(xgenGD.KitchenSink);
        }
    }
    /***************************************************************
      * Get a temporary filename for the error output.
     **************************************************************/
    com->errfile = XtNewString(tmpnam(NULL));

    /***************************************************************
      * Flush out any buffered data.
     **************************************************************/
    fflush(stdout);
    fflush(stderr);

    /***************************************************************
      * Is the user slipping an '&' by us??
     **************************************************************/
    if ((ampersand = (char *) strrchr(command, '&')) != NULL) {
        com->dowait = False;
        *ampersand = '\0';
    }
    if ( !com->dowait ) (void) signal(SIGCHLD,KidDied);
    /***************************************************************
      * Fork.
     **************************************************************/
    switch (child = fork()) {
    case -1:                    /* Error */
        perror("fork");
    case 0:                     /* The Child */

        /* if we're doing input dup stdin */
        if (com->input)
            DoDup(com->source, 0);

        /* if we're capturing output */
        if (com->capture) {
            /* in an editor area */
            if (!strcmp(com->sink, "editor")) {
                DoDup(com->tmpfile, 1);
            } else if (!strcmp(com->sink, "append")) {
                DoDupSkee(xgenGD.KitchenSink, 1);

                /* in a file */
            } else {
                DoDup(com->sink, 1);
            }
        } else {
            DoDup("/dev/tty", 1);
        }
        DoDup(com->errfile, 2);
        /*************************************************************
        if the user has defined a command interpreter fork it
        otherwise fork /bin/sh
        **************************************************************/
        if (!strcmp(com->path, "KillInterActive")) {
            fprintf(stderr, "interactive command is dead\n");
        } else {
            if (com->interp == True) {
                execlp(com->cinterp, com->cinterp, command, 0);
            } else {
                execl("/bin/sh", "sh", "-c", command, 0);
            }
        }
        exit(127);
    default:                    /* The parent */
        actual_kid = child;
        break;
    }
    /***************************************************************
     * if this is a foreground process, wait for it
     **************************************************************/
    if (com->dowait) {
        PIDTYPE                         w;

#ifdef SVR4
        while ( (w = waitpid(actual_kid,&status,WNOHANG)) != actual_kid &&
                 w != -1 ) ;
#else
#ifdef BSD
        while ( (w = wait3(&status,WNOHANG, NULL)) != actual_kid &&
                 w != -1 ) ;
#else
        while ( (w = waitpid(actual_kid,&status,WNOHANG)) != actual_kid &&
                 w != -1 ) ;
#endif
#endif

        /* Check for errors */
#ifdef SVR4
        if (WIFEXITED(status) != 0 && WEXITSTATUS(status) != 0) {
#else /* SVR4 */
#ifdef BSD
        if (WIFEXITED(status) != 0 && status.w_retcode != 0) {
#else /* BSD */
        if (WIFEXITED(status) != 0 && WEXITSTATUS(status) != 0) {
#endif /* BSD */
#endif /* SVR4 */
            Boolean                         acceptable = False;
            /*
             * for  ( i = 0; i < nerr; i++ ) { if ( WEXITSTATUS(status) ==
             * err[i] ) { acceptable = True; } }
             */
#ifdef SVR4
#else /* SVR4 */
#ifdef BSD
            if (status.w_retcode == 0)
#else /* BSD */
            if (WEXITSTATUS(status) == 0)
#endif /* BSD */
#endif /* SVR4 */
                acceptable = True;
            if (!acceptable) {
                DoError(com);
                unlink(com->errfile);
                if (com->capture && !strcmp(com->sink, "editor"))
                    unlink(com->tmpfile);
		if (resBlack && resWhite) {
		    Shell                          *s = xgenGD.currentEnv->shells;
		    while (s) {
			if (s->popup)
			    XUndefineCursor(xgenGD.display, XtWindow(s->widget));
			s = s->next;
		    }
		    XFreeCursor(xgenGD.display, hourglassCursor);
		}
                return;
            } else {
                if (com->capture && !strcmp(com->sink, "editor")) {
                    DoCapture(com);
                    unlink(com->tmpfile);
                }
            }
        } else {
            if (com->capture && !strcmp(com->sink, "editor")) {
                DoCapture(com);
                unlink(com->tmpfile);
            } else if (com->capture && (!strcmp(com->sink, "append"))) {
                DocaptureAll(com);
            }
        }
    } else {
        int                             ret;
        /***************************************************************
         * update the pid field and stash the structure away.
         **************************************************************/
        com->pid = actual_kid;
#ifdef SVR4
        ret = setpgid(com->pid, 0);
        if (ret == -1)
            perror("setpgid");
#else
#ifdef BSD
        ret = setpgrp(com->pid, com->pid);
        if (ret == -1)
            perror("setpgrp");
#else
        ret = setpgid(com->pid, 0);
        if (ret == -1)
            perror("setpgid");
#endif
#endif
        AddCommand(com);
        if (!nocontrol)
            AddCommandToControlBox(com);
    }
    if (com->dowait && resBlack && resWhite) {
        Shell                          *s = xgenGD.currentEnv->shells;
        while (s) {
            if (s->popup)
                XUndefineCursor(xgenGD.display, XtWindow(s->widget));
            s = s->next;
        }
        XFreeCursor(xgenGD.display, hourglassCursor);
    }
}

#ifdef BSD
int 
#else
void 
#endif
KidDied()
{
#ifdef PRE_MOTIF_1_1
	    XtAddTimeOut(xgenGD.appContext, 0, DoFinishCommand, NULL);
#else
	    XtAppAddTimeOut(xgenGD.appContext, 0, (XtTimerCallbackProc) DoFinishCommand, NULL);
#endif
}
