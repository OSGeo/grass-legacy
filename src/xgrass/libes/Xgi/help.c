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
#ifdef SVR4
#define XmSCROLL_BAR_BIT XmSCROLLBAR_BIT
#endif
#ifdef OKI
#define XmSCROLL_BAR_BIT XmSCROLLBAR_BIT
#endif
#if defined(mips) && !defined(sgi)
#define XmSCROLL_BAR_BIT XmSCROLLBAR_BIT
#endif
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

#include <ctype.h>
#include <stdio.h>
#include <sys/stat.h>

/****************************************************************/
void
#ifdef _NO_PROTO
XgHelpCallback(widget, client_data, call_data)
    Widget                          widget;
    XtPointer                       client_data;
    XtPointer                       call_data;
#else
XgHelpCallback(Widget widget,
               XtPointer client_data,
               XtPointer call_data)
#endif
{
    char                           *help_string = (char *) client_data;
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
    xms = XmStringCreateSimple("XGRASS Context Help");

    XtSetArg(al[ac], XmNdialogTitle, xms);
    ac++;
    w = XmCreateInformationDialog(widget, "context_help", al, ac);

    xms1 = XmStringCreateLtoR(help_string,XmSTRING_DEFAULT_CHARSET);
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

void
#ifdef _NO_PROTO
XgHelpCallbackFromFile(widget, client_data, call_data)
    Widget                          widget;
    XtPointer                       client_data;
    XtPointer                       call_data;
#else
XgHelpCallbackFromFile(Widget widget,
               XtPointer client_data,
               XtPointer call_data)
#endif
{
    char                           *file = (char *) client_data;
    XmString                        xms, xms1, xms2;
    Widget                          w;
    Arg                             al[5];
    int                             ac = 0;
    Widget                          workarea;
    Widget                          text;
    Widget                          prompt;
    char *helpDir;
    char path[1024];
    FILE *fp, *fopen();
    struct stat statbuf;
    char *string, *ptr;
    int length;

    if ( (helpDir = (char *)getenv("XGRASSHELPDIR")) == NULL ) {
        XgWarningDialog(w, "XGRASSHELPDIR not set....");
        return;
    }

    while (widget != NULL && XmIsGadget(widget)) {
        widget = XtParent(widget);
    }
    if (widget == NULL) {
        return;
    }
    xms = XmStringCreateSimple("XGRASS Context Help");

    XtSetArg(al[ac], XmNdialogTitle, xms);
    ac++;
    w = XmCreateInformationDialog(widget, "context_help", al, ac);

    strcpy(path, helpDir);
    strcat(path, "/xgrass/");
    strcat(path, file);


    if ((fp = fopen(path,"r")) == NULL ) {
        char errorbuf[1024];

        sprintf(errorbuf,"Can't open context help file:\n\"%s\"",path);
        XgWarningDialog(widget, errorbuf);
        return;
    }
    if ( stat(path,&statbuf) == 0 )
        length = statbuf.st_size;
    else
        length = 10000000;

    if ( length > 0 ) {
	int bytes;

        string = (char *)XtMalloc(length);
        bytes = fread(string,sizeof(char), length, fp);
	string[bytes] = '\0';
    }
    ptr = string;
    while ( *ptr ) {
        if ( *ptr == '\n' )
            *ptr = '\012';
        else if ( !isprint(*ptr) )
            *ptr = ' ';
        ptr++;
    }
    fclose(fp);


    xms1 = XmStringCreateLtoR(string,XmSTRING_DEFAULT_CHARSET);
    xms2 = XmStringCreateSimple("Ok");

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



#ifdef _NO_PROTO
_XgSupplyGenericHelp(parent, w)
    Widget                          parent;
    Widget                          w;
#else
_XgSupplyGenericHelp(Widget parent, Widget w)
#endif
{
    char                           *generic_help;
    /*
     * Try our widgets first...
     */
    if (XgIsInteractor(w)) {
        generic_help =
            XtNewString("This is an Interactor, select \"Help on Motif\" in the XGRASS main menu for more help.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XgIsPixel(w)) {
        generic_help =
            XtNewString("This is a Pixel editor, used for changing a single color.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    /*
     * Try motif widgets...
     */
    if (XmIsArrowButton(w) || XmIsArrowButtonGadget(w)) {
        generic_help =
            XtNewString("This is an arrow button, click with your left mouse button to activate.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsCascadeButton(w) || XmIsCascadeButtonGadget(w)) {
        generic_help =
            XtNewString("This is a cascade button, clicking with your left mouse button will activate a pulldown menu.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsDrawnButton(w)) {
        generic_help =
            XtNewString("This is a drawn button, click with your left mouse button to activate.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsPushButton(w) || XmIsPushButtonGadget(w)) {
        generic_help =
            XtNewString("This is a push button, click with your left mouse button to activate.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsLabel(w) || XmIsLabelGadget(w)) {
        generic_help =
            XtNewString("This is a label, it has no action associated with it.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsList(w)) {
        generic_help =
            XtNewString("This is a list, click on one of the list items make a selection.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsScale(w)) {
        generic_help =
            XtNewString("This is a scale widget, press your left mouse button on the scale handle then drag to the desired value.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsScrollBar(w)) {
        generic_help =
            XtNewString("This is a scrollbar, press your left mouse button on the handle then drag to scroll. Click on the arrow buttons with your left mouse button to move in small increments.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsSeparator(w) || XmIsSeparatorGadget(w)) {
        generic_help =
            XtNewString("Tis is a separator, how the heck did you select this little thing??");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsText(w)) {
        generic_help =
            XtNewString("This is a text area, select \"Help on Motif\" in the XGRASS main menu for more help.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsTextField(w)) {
        generic_help =
            XtNewString("This is a text area, select \"Help on Motif\" in the XGRASS main menu for more help.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsToggleButton(w) || XmIsToggleButtonGadget(w)) {
        generic_help =
            XtNewString("This is a toggle button, click with your left mouse button to change the state of this button.");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsFrame(w)) {
        generic_help =
            XtNewString("This is where generic help will go...");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    if (XmIsMainWindow(w)) {
        generic_help =
            XtNewString("This is where generic help will go...");

        XgHelpCallback(parent, (XtPointer) generic_help, NULL);
        return 1;
    }
    return 0;
}


/* Pass this routine a widget as the first argument, and any number
 of C strings followed by NULL */
void
XgAddHelpCallback(va_alist)
va_dcl
{
    size_t                          len = 0;
    char                           *retbuf;
    va_list                         argp;
    char                           *p;
    Widget w;


    va_start(argp);
    w = va_arg(argp,Widget);

    while ((p = va_arg(argp, char *)) != NULL)
        len += strlen(p) + 1; /* +1 for space */

    va_end(argp);

   retbuf = XtMalloc(len + 1);   /* +1 for trailing \0 */

    *retbuf = 0;

    va_start(argp);
    w = va_arg(argp,Widget);

    while ((p = va_arg(argp, char *)) != NULL) {
        (void) strcat(retbuf, p);
        (void) strcat(retbuf, " ");
    }

    va_end(argp);

    XtAddCallback(w, XmNhelpCallback, XgHelpCallback,
                  (XtPointer) XtNewString(retbuf));

}

void 
XgAddHelpCallBackFromFile(w, file)
Widget w;
char *file;
{
    XtAddCallback(w, XmNhelpCallback, XgHelpCallbackFromFile,
                  (XtPointer) XtNewString(file));
}
