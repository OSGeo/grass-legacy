#include <Interact.h>
#include <Pixel.h>
#include <Xm/XmP.h>
#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/Command.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/SelectioB.h>
#include <Xm/SeparatoG.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <varargs.h>

#include <stdio.h>

/****************************************************************/
void
#ifdef _NO_PROTO
XgError(widget, string)
    Widget                          widget;
    char *string;
#else
XgError(Widget widget,
               char *string)
#endif
{
    XmString                        xms, xms1, xms2;
    Widget                          w;
    Arg                             al[5];
    int                             ac = 0;
    Widget                          workarea;
    Widget                          text;
    Widget                          prompt;

    while (widget != NULL && XmIsGadget(widget)) {
        widget = XtParent(widget);
    }
    if (widget == NULL) {
        return;
    }
    xms = XmStringCreateSimple("XGRASS Error");

    XtSetArg(al[ac], XmNdialogTitle, xms);
    ac++;
    w = XmCreateInformationDialog(widget, "error_dialog", al, ac);

    xms1 = XmStringCreateLtoR(string,XmSTRING_DEFAULT_CHARSET);
    xms2 = XmStringCreateSimple("OK");

    XtVaSetValues(w,
                  XmNmessageString, xms1,
                  XmNokLabelString, xms2,
                  NULL);
    XmStringFree(xms1);
    XmStringFree(xms2);
    XtUnmanageChild(XmMessageBoxGetChild(w, XmDIALOG_HELP_BUTTON));
    XtUnmanageChild(XmMessageBoxGetChild(w, XmDIALOG_CANCEL_BUTTON));

    XtManageChild(w);
}
