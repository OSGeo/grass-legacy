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

    if ( goner != NULL ) {
        XtUnrealizeWidget(goner->widget);
        XtDestroyWidget(goner->widget);
    }
}

void 
ControlBoxQuit()
{
    static Widget qdialog;
    static void Quit();
    static void Cancel();

    XgenExit(0);
/*
    if (qdialog) {
        XtManageChild(qdialog);    
    } else {
        char buf[80];
        int n,height,width,x,y;

        n = 0;
        sprintf(buf,"Quit %s ?",xgenGD.progName);
        XtSetArg(args[n],XmNx,DisplayWidth(xgenGD.display,xgenGD.screen)/2); n++;
        XtSetArg(args[n],XmNy,Displayheight(xgenGD.display,xgenGD.screen)/2); n++;
        XtSetArg(args[n],XmNmessageString,XmStringCreate(buf,SDC)); n++;
        qdialog = XmCreateQuestionDialog(xgenGD.applShell,"quit confirmer",args,n);
        XtManageChild(qdialog);
        XtAddCallback(qdialog,XmNokCallback,Quit,NULL); 
        XtAddCallback(qdialog,XmNcancelCallback,Cancel,NULL);
        XtUnmanageChild(XmMessageBoxGetChild(qdialog,XmDIALOG_HELP_BUTTON));

        n = 0;
        XtSetArg(args[n], XmNheight, &height); n++;
        XtSetArg(args[n], XmNwidth, &width); n++;
        XtGetValues (qdialog, (ArgList) args, n);

        x = (DisplayWidth (xgenGD.display, xgenGD.screen) - ((int) width))/2;
        y = (DisplayHeight (xgenGD.display, xgenGD.screen) - ((int) height))/2;
        n = 0;
        XtSetArg(args[n], XmNx, (XtArgVal) x); n++;
        XtSetArg(args[n], XmNy, (XtArgVal) y); n++;
        XtSetValues (qdialog, (ArgList) args, n);
    }
    */
}

static void
Quit(w,cld,cad)
    Widget w;
    caddr_t cld;
    caddr_t cad;
{
    XgenExit(0);
}

static void
Cancel(w,cld,cad)
    Widget w;
    caddr_t cld;
    caddr_t cad;
{
    XtUnmanageChild(w);
}

static void 
ForegroundCB(w,cld,cad)
    Widget w;
    caddr_t cld;
    caddr_t cad;
{
    Command *command = (Command *)cld;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)cad;
    char s[80];
    union wait status;
    int pid;
    int pgrp;

    while ( (pid = wait(&status)) != command->pid && pid != -1 ) {
        DoReap(status,pid);
    }
    if ( pid != -1 )
        DoReap(status,pid);
    return;
}

static void 
KillCB(w,cld,cad)
    Widget w;
    caddr_t cld;
    caddr_t cad;
{
    Command *command = (Command *)cld;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)cad;
    char s[80];
    union wait status;
    int pid;
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
