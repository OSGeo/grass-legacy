/**********************************************************************
   control.c    - perform control box functions
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

void                            KillCB();
static void                     BringUpEnv();

void
MakeControlBox()
{
    Widget                          separator1, separator2, cbLabel1, cbLabel2,
                                    quitButton, button;
    int                             n;
    char                            buf[80];
    XmString                        xmLabel;
    void                            ControlBoxQuit();
    Environ                        *e;

    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    XtSetArg(args[n], XmNmarginWidth, 0);
    n++;
    XtSetArg(args[n], XmNmarginHeight, 0);
    n++;
    XtSetArg(args[n], XmNpacking, XmPACK_COLUMN);
    n++;
    XtSetArg(args[n], XmNorientation, XmVERTICAL);
    n++;
    xgenGD.CBRowCol = XmCreateRowColumn(xgenGD.applShell, "controlbox pane", args, n);
    XtManageChild(xgenGD.CBRowCol);

    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    sprintf(buf, "%s Control Box", xgenGD.progName);
    xmLabel = XmStringLtoRCreate(buf, SDC);
    XtSetArg(args[n], XmNlabelType, XmSTRING);
    n++;
    XtSetArg(args[n], XmNlabelString, xmLabel);
    n++;
    cbLabel1 = XmCreateLabelGadget(xgenGD.CBRowCol, "label", args, n);
    XtManageChild(cbLabel1);
    XmStringFree(xmLabel);

    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    xmLabel = XmStringLtoRCreate("Quit", SDC);
    XtSetArg(args[n], XmNlabelType, XmSTRING);
    n++;
    XtSetArg(args[n], XmNlabelString, xmLabel);
    n++;
    quitButton = XmCreatePushButtonGadget(xgenGD.CBRowCol, "exit", args, n);
    XtManageChild(quitButton);
    XmStringFree(xmLabel);

    XtAddCallback(quitButton, XmNactivateCallback, ControlBoxQuit, NULL);

    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    XtSetArg(args[n], XmNorientation, XmHORIZONTAL);
    n++;
    XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN);
    n++;
    separator1 = XmCreateSeparator(xgenGD.CBRowCol, "s1", args, n);
    XtManageChild(separator1);

    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    sprintf(buf, "Environments: %s", xgenGD.progName);
    xmLabel = XmStringLtoRCreate(buf, SDC);
    XtSetArg(args[n], XmNlabelType, XmSTRING);
    n++;
    XtSetArg(args[n], XmNlabelString, xmLabel);
    n++;
    cbLabel2 = XmCreateLabelGadget(xgenGD.CBRowCol, "label", args, n);
    XtManageChild(cbLabel2);
    XmStringFree(xmLabel);

    for ( e = xgenGD.toplevelEnv; e; e = e->next) {
	n = 0;
	SetGlobalArgs(&n, FONTS);
	SetObjectColorArgs(NULL, &n);
	xmLabel = XmStringLtoRCreate(e->name, SDC);
	XtSetArg(args[n], XmNlabelType, XmSTRING);
	n++;
	XtSetArg(args[n], XmNlabelString, xmLabel);
	n++;
	button = XmCreatePushButtonGadget(xgenGD.CBRowCol, "exit", args, n);
	XtManageChild(button);
	XtAddCallback(button, XmNactivateCallback, BringUpEnv, e);
	XmStringFree(xmLabel);
    }

    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    XtSetArg(args[n], XmNorientation, XmHORIZONTAL);
    n++;
    XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN);
    n++;
    separator1 = XmCreateSeparator(xgenGD.CBRowCol, "s1", args, n);
    XtManageChild(separator1);

    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    sprintf(buf, "Background Jobs: %s", xgenGD.progName);
    xmLabel = XmStringLtoRCreate(buf, SDC);
    XtSetArg(args[n], XmNlabelType, XmSTRING);
    n++;
    XtSetArg(args[n], XmNlabelString, xmLabel);
    n++;
    cbLabel2 = XmCreateLabelGadget(xgenGD.CBRowCol, "label", args, n);
    XtManageChild(cbLabel2);
    XmStringFree(xmLabel);

    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    XtSetArg(args[n], XmNorientation, XmHORIZONTAL);
    n++;
    XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN);
    n++;
    separator2 = XmCreateSeparator(xgenGD.CBRowCol, "s2", args, n);
    XtManageChild(separator2);

}

void
AddCommandToControlBox(com)
    Command                        *com;
{
    Widget                          button;
    XmString                        xmLabel;
    int                             n;
    char                            buf[1024];

    sprintf(buf, "Kill: %s", com->path);
    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectColorArgs(NULL, &n);
    xmLabel = XmStringLtoRCreate(buf, SDC);
    XtSetArg(args[n], XmNlabelString, xmLabel);
    n++;
    button = XmCreatePushButtonGadget(xgenGD.CBRowCol, "kill_button", args, n);
    XtManageChild(button);
    XmStringFree(xmLabel);

    com->widget = button;

    XtAddCallback(button, XmNactivateCallback, KillCB, com);

}

void
DeleteCommandFromControlBox(pid)
    PIDTYPE                         pid;
{
    Command                        *goner = FindCommand(pid);

    if (!nocontrol)
        if (goner != NULL && XtIsRealized(goner->widget)) {
            XtDestroyWidget(goner->widget);
        }
}

void
ControlBoxQuit()
{
    XgenExit(0);
}

void
KillCB(w, cld, cad)
/* ARGSUSED */
    Widget                          w;
    caddr_t                         cld;
    caddr_t                         cad;
{
    Command                        *command = (Command *) cld;

    DeleteCommandFromControlBox(command->pid);
    /* unlink temp files if they exist */
    if (command->capture && !access(command->tmpfile, 0))
        unlink(command->tmpfile);
#ifdef SVR4
    kill(-command->pid, SIGTERM);
#else
#if defined(SUNOS) || defined(BSD) || defined(AIX)
    killpg(command->pid, SIGTERM);
#else
    kill(-command->pid, SIGTERM);
#endif
#endif
    DeleteCommand(command);
    return;
}

static void
BringUpEnv(w, cld, cad)
/* ARGSUSED */
    Widget                          w;
    caddr_t                         cld;
    caddr_t                         cad;
{
    Environ                        *e = (Environ *) cld;

    PopupEnviron(e);
    return;
}
