
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#define ASCII_STRING
#include <X11/Xaw/AsciiText.h>
#include <X11/Xatom.h>
#include "gis.h"

#define nActions(x)     (sizeof(x)/sizeof(XtActionsRec))

Widget params, foo, titl, n, s, w, e, nsres, ewres;
Widget nt, st, wt, et, nst, ewt;

struct Cell_head window;
extern Display *dpy;
extern XtTranslations form_trans;
extern Cursor cur1;
void set_window();
static int data_win_flag = 0;
static XtCallbackRec set_win_callback[] = {
    {set_window, NULL},
    {NULL, NULL}
};

extern char n_str[12], s_str[12], w_str[12], e_str[12];
extern char nsres_str[12], ewres_str[12];
extern Atom north, south, east, west, ns, ew;


void change_window(wind, client_data, call_data)
Widget wind;
caddr_t client_data;
caddr_t call_data;
{
    Arg arglist[20];
    extern XFontStruct *font2, *font3, *font1;

    if (data_win_flag == 1)
        return;


    XtSetArg(arglist[0], XtNminWidth, 342);
    XtSetArg(arglist[1], XtNminHeight, 150);
    XtSetArg(arglist[2], XtNheight, 150);
    XtSetArg(arglist[3], XtNwidth, 342);
    XtSetArg(arglist[4], XtNmaxWidth, 342);
    XtSetArg(arglist[5], XtNmaxHeight, 150);
    XtSetArg(arglist[6], XtNiconName, "database window");


    params = XtCreateApplicationShell("params",
        topLevelShellWidgetClass,
        arglist, 7);

    XtSetArg(arglist[0], XtNheight, 150);
    XtSetArg(arglist[1], XtNwidth, 342);
    XtSetArg(arglist[2], XtNbackground,
        pixel("white"));




    foo = XtCreateManagedWidget("form_window",
        formWidgetClass, params,
        arglist, 3);

    XtOverrideTranslations(foo, form_trans);


    XtSetArg(arglist[0], XtNheight, 20);
    XtSetArg(arglist[1], XtNwidth, 342);
    XtSetArg(arglist[2], XtNlabel,
        "DATABASE WINDOW");
    XtSetArg(arglist[3], XtNfont, font1);
    XtSetArg(arglist[4], XtNbackground,
        pixel("red"));
    XtSetArg(arglist[5], XtNforeground,
        pixel("yellow"));


    titl = XtCreateManagedWidget("titl",
        labelWidgetClass, foo,
        arglist, 6);

    XtSetArg(arglist[0], XtNheight, 20);
    XtSetArg(arglist[1], XtNwidth, 83);
    XtSetArg(arglist[2], XtNborderColor, pixel("blue"));
    XtSetArg(arglist[3], XtNfromVert, titl);
    XtSetArg(arglist[4], XtNbackground, pixel("wheat"));
    XtSetArg(arglist[5], XtNfont, font3);
    XtSetArg(arglist[6], XtNforeground, pixel("black"));

    n = XtCreateManagedWidget("NORTH  :",
        labelWidgetClass, foo,
        arglist, 7);
    XtSetArg(arglist[4], XtNbackground, pixel("white"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[7], XtNfromHoriz, n);
    XtSetArg(arglist[8], XtNstring, n_str);
    XtSetArg(arglist[9], XtNeditType, XawtextEdit);
    XtSetArg(arglist[10], XtNlength, 12);
    XtSetArg(arglist[11], XtNhorizDistance, 0);
    XtSetArg(arglist[12], XtNhighlightThickness, 15);
    XtSetArg(arglist[13], XtNleftMargin, 6);
    XtSetArg(arglist[14], XtNinsertPosition, strlen(n_str));
    XtSetArg(arglist[15], XtNcursor, cur1);

    nt = XtCreateManagedWidget("", asciiStringWidgetClass, foo,
        arglist, 16);

    XtSetArg(arglist[4], XtNbackground, pixel("wheat"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[7], XtNfromHoriz, nt);
    w = XtCreateManagedWidget("WEST   :", labelWidgetClass, foo,
        arglist, 8);
    XtSetArg(arglist[4], XtNbackground, pixel("white"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[7], XtNfromHoriz, w);
    XtSetArg(arglist[8], XtNstring, w_str);
    XtSetArg(arglist[14], XtNinsertPosition, strlen(w_str));

    wt = XtCreateManagedWidget("", asciiStringWidgetClass, foo,
        arglist, 16);

    XtSetArg(arglist[4], XtNbackground, pixel("wheat"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[3], XtNfromVert, n);
    s = XtCreateManagedWidget("SOUTH  :",
        labelWidgetClass, foo,
        arglist, 7);

    XtSetArg(arglist[4], XtNbackground, pixel("white"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[7], XtNfromHoriz, s);
    XtSetArg(arglist[8], XtNstring, s_str);
    XtSetArg(arglist[14], XtNinsertPosition, strlen(s_str));

    st = XtCreateManagedWidget("",
        asciiStringWidgetClass, foo,
        arglist, 16);

    XtSetArg(arglist[4], XtNbackground, pixel("wheat"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[7], XtNfromHoriz, st);
    e = XtCreateManagedWidget("EAST   :",
        labelWidgetClass, foo,
        arglist, 8);

    XtSetArg(arglist[4], XtNbackground, pixel("white"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[7], XtNfromHoriz, e);
    XtSetArg(arglist[8], XtNstring, e_str);
    XtSetArg(arglist[14], XtNinsertPosition, strlen(n_str));

    et = XtCreateManagedWidget("",
        asciiStringWidgetClass, foo,
        arglist, 16);

    XtSetArg(arglist[4], XtNbackground, pixel("wheat"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[3], XtNfromVert, s);
    nsres = XtCreateManagedWidget("NSRES  :",
        labelWidgetClass, foo,
        arglist, 7);

    XtSetArg(arglist[4], XtNbackground, pixel("white"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[7], XtNfromHoriz, nsres);
    XtSetArg(arglist[8], XtNstring, nsres_str);
    XtSetArg(arglist[14], XtNinsertPosition, strlen(n_str));

    nst = XtCreateManagedWidget("",
        asciiStringWidgetClass, foo,
        arglist, 16);

    XtSetArg(arglist[4], XtNbackground, pixel("wheat"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[7], XtNfromHoriz, nst);
    ewres = XtCreateManagedWidget("EWRES  :",
        labelWidgetClass, foo,
        arglist, 8);

    XtSetArg(arglist[4], XtNbackground, pixel("white"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[7], XtNfromHoriz, ewres);
    XtSetArg(arglist[8], XtNstring, ewres_str);
    XtSetArg(arglist[14], XtNinsertPosition, strlen(n_str));

    ewt = XtCreateManagedWidget("",
        asciiStringWidgetClass, foo,
        arglist, 16);

    XtSetArg(arglist[0], XtNheight, 20);
    XtSetArg(arglist[1], XtNwidth, 342);
    XtSetArg(arglist[2], XtNfromVert, nsres);
    XtSetArg(arglist[3], XtNbackground, pixel("blue"));
    XtSetArg(arglist[4], XtNforeground, pixel("yellow"));
    XtSetArg(arglist[5], XtNhighlightThickness, 15);
    XtSetArg(arglist[6], XtNfont, font2);
    XtSetArg(arglist[7], XtNcallback, set_win_callback);



    nt = XtCreateManagedWidget("SET WINDOW",
        commandWidgetClass, foo,
        arglist, 8);

    XtRealizeWidget(params);
    data_win_flag = 1;

}

void set_window(widget, event, params, nparams)
Widget widget;
XEvent *event;
char **params;
int *nparams;
{
    extern void set_win_info();

    set_win_info();
    XtDestroyWidget(XtParent(foo));
    data_win_flag = 0;
}
