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

void KillCB();
void ForegroundCB();

MakeControlBox()
{
    Widget separator1, separator2, cbLabel1, cbLabel2, quitButton;
    int n;
    char buf[80];
    XmString xmLabel;
    void ControlBoxQuit();

    n = 0;
    SetGlobalArgs(&n,FONTS);
    SetObjectColorArgs(NULL,&n);
    XtSetArg(args[n],XmNmarginWidth,0); n++;
    XtSetArg(args[n],XmNmarginHeight,0); n++;
    XtSetArg(args[n],XmNpacking,XmPACK_COLUMN); n++;
    XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
    xgenGD.CBRowCol = XmCreateRowColumn(xgenGD.applShell,"controlbox pane",args,n);
    XtManageChild(xgenGD.CBRowCol);

    n = 0;
    SetGlobalArgs(&n,FONTS);
    SetObjectColorArgs(NULL,&n);
    sprintf(buf,"%s Control Box",xgenGD.progName);
    xmLabel = XmStringLtoRCreate(buf,SDC);
    XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
    XtSetArg(args[n],XmNlabelString,xmLabel); n++;
    cbLabel1 = XmCreateLabelGadget(xgenGD.CBRowCol,"label",args,n);
    XtManageChild(cbLabel1);
	XmStringFree(xmLabel);

    n = 0;
    SetGlobalArgs(&n,FONTS);
    SetObjectColorArgs(NULL,&n);
    xmLabel = XmStringLtoRCreate("Quit",SDC);
    XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
    XtSetArg(args[n],XmNlabelString,xmLabel); n++;
    XtSetArg(args[n],XmNborderWidth,10); n++;
    quitButton = XmCreatePushButtonGadget(xgenGD.CBRowCol,"exit",args,n);
    XtManageChild(quitButton);
	XmStringFree(xmLabel);

    XtAddCallback(quitButton, XmNactivateCallback, ControlBoxQuit, NULL);

    n = 0;
    SetGlobalArgs(&n,FONTS);
    SetObjectColorArgs(NULL,&n);
    XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
    XtSetArg(args[n],XmNseparatorType,XmSHADOW_ETCHED_IN); n++;
    separator1 = XmCreateSeparator(xgenGD.CBRowCol,"s1",args,n);
    XtManageChild(separator1);

    n = 0;
    SetGlobalArgs(&n,FONTS);
    SetObjectColorArgs(NULL,&n);
    sprintf(buf,"Background Jobs:",xgenGD.progName);
    xmLabel = XmStringLtoRCreate(buf,SDC);
    XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
    XtSetArg(args[n],XmNlabelString,xmLabel); n++;
    cbLabel2 = XmCreateLabelGadget(xgenGD.CBRowCol,"label",args,n);
    XtManageChild(cbLabel2);
	XmStringFree(xmLabel);

    n = 0;
    SetGlobalArgs(&n,FONTS);
    SetObjectColorArgs(NULL,&n);
    XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
    XtSetArg(args[n],XmNseparatorType,XmSHADOW_ETCHED_IN); n++;
    separator2 = XmCreateSeparator(xgenGD.CBRowCol,"s2",args,n);
    XtManageChild(separator2);

}

void 
AddCommandToControlBox(com)
    Command *com;
{
    Widget optMenu;
    Widget optSubmenu;
    Widget opts[2];
    XmString xmLabel;
    int n;

    n = 0;
    SetGlobalArgs(&n,FONTS);
    SetObjectColorArgs(NULL,&n);
    optSubmenu = XmCreatePulldownMenu(xgenGD.CBRowCol,"optionsubmenu",args,n);

	n = 0;
    SetGlobalArgs(&n,FONTS);
    SetObjectColorArgs(NULL,&n);
    xmLabel = XmStringLtoRCreate("foreground",SDC);
    XtSetArg(args[n],XmNlabelString,xmLabel); n++;
    opts[0] = XmCreatePushButtonGadget(optSubmenu,"option1",args,n);
    XtManageChild(opts[0]);
    XmStringFree(xmLabel);

	n = 0;
    SetGlobalArgs(&n,FONTS);
    SetObjectColorArgs(NULL,&n);
    xmLabel = XmStringLtoRCreate("kill",SDC);
    XtSetArg(args[n],XmNlabelString,xmLabel); n++;
    opts[1] = XmCreatePushButtonGadget(optSubmenu,"option2",args,n);
    XtManageChild(opts[1]);
    XmStringFree(xmLabel);

    n = 0;
    SetGlobalArgs(&n,FONTS);
    SetObjectColorArgs(NULL,&n);
    xmLabel = XmStringLtoRCreate(com->path,SDC);
    XtSetArg(args[n],XmNlabelString,xmLabel); n++;
    XtSetArg(args[n],XmNsubMenuId,optSubmenu); n++;
    optMenu = XmCreateOptionMenu(xgenGD.CBRowCol,"command",args,n);
    XtManageChild(optMenu);
    XmStringFree(xmLabel);

	com->widget = optMenu;

    XtAddCallback(opts[0], XmNactivateCallback, ForegroundCB, com);
    XtAddCallback(opts[1], XmNactivateCallback, KillCB, com);
    
} 

void 
DeleteCommandFromControlBox(pid)
    int pid;
{
    Command *goner = FindCommand(pid);

	if ( !nocontrol )
        if ( goner != NULL ) {
            XtUnrealizeWidget(goner->widget);
            XtDestroyWidget(goner->widget);
        }
}

void 
ControlBoxQuit()
{
    XgenExit(0);
}

static void 
ForegroundCB(w,cld,cad)
	/*ARGSUSED*/
    Widget w;
    caddr_t cld;
    caddr_t cad;
{
    Command *command = (Command *)cld;
    union wait status;
    int pid;

    while ( (pid = wait(&status)) != command->pid && pid != -1 ) {
        DoReap(status,pid);
    }
    if ( pid != -1 )
        DoReap(status,pid);
    return;
}

static void 
KillCB(w,cld,cad)
	/*ARGSUSED*/
    Widget w;
    caddr_t cld;
    caddr_t cad;
{
    Command *command = (Command *)cld;
    char s[80];
    int pgrp;

    /* unlink temp files if they exist */
    if ( command->capture && !access(command->tmpfile,0) )
        unlink(command->tmpfile);
    if ( !access(command->errfile,0) )
        unlink(command->errfile);
    /* check for existance */
    if ( kill(command->pid,0) == 0 && errno != ESRCH ) {
        /* get the process group and wipe it out */
        pgrp = getpgrp(command->pid);
        if ( killpg(pgrp,SIGINT) < 0 ) {
            sprintf(s,"killpg: process group %d",pgrp);
            perror(s);
        }
    }
    DeleteCommandFromControlBox(command->pid);
    DeleteCommand(command->pid);
    return;
}
