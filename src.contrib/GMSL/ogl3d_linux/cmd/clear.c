
/* clear:
** this function clears the text widget that is used to show info when the 
** "whats here?" button is active
*/



#include "interface.h"

void 
cleartxt (Widget w, data_cell *dc, caddr_t client_data)
{
    Arg wargs[15];
    int n;


    XtDestroyWidget(dc->Wtxt);

    /*------ re-make the text widget to display the info ------*/
    n = 0;
    XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_POSITION); n++;
    XtSetArg(wargs[n],XmNtopPosition,20); n++;
    XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_POSITION); n++;
    XtSetArg(wargs[n],XmNbottomPosition,78); n++;
    XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_POSITION); n++;
    XtSetArg(wargs[n],XmNleftPosition,3); n++;
    XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_POSITION); n++;
    XtSetArg(wargs[n],XmNrightPosition,98); n++;
    XtSetArg(wargs[n],XmNeditable,FALSE); n++;
    XtSetArg(wargs[n],XmNwordWrap,TRUE); n++;
    XtSetArg(wargs[n],XmNeditMode,XmMULTI_LINE_EDIT); n++;
    XtSetArg(wargs[n],XmNscrollingPolicy, XmAUTOMATIC); n++;
    dc->Wtxt = XmCreateScrolledText(dc->panels[WHAT].form, "Wtxt", wargs, n);
    XtManageChild(dc->Wtxt);


    /*---  reset the current text position ---*/
    dc->current_position = 0;
	
}

