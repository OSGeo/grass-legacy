
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#define ASCII_STRING
#include <X11/Xaw/AsciiText.h>
#include "gis.h"

#define nActions(x)     (sizeof(x)/sizeof(XtActionsRec))

Widget eparams, efoo, etitl, mapset, mt, location, lt;
Widget gisdbase, gt, setenv;
char m_str[25], l_str[25], g_str[25];
struct Cell_head window;
extern Display *dpy;
extern XtTranslations form_trans;
extern Cursor cur1;
void set_environment();
static int env_win_flag = 0;
static XtCallbackRec set_env_callback[] = {
    {set_environment, NULL},
    {NULL, NULL}
};


void change_environment(wind, client_data, call_data)
Widget wind;
caddr_t client_data;
caddr_t call_data;
{
    Arg arglist[20];
    extern XFontStruct *font2, *font3, *font1;

    if (env_win_flag == 1)
        return;
    read_env_info(m_str, l_str, g_str);

    XtSetArg(arglist[0], XtNminWidth, 225);
    XtSetArg(arglist[1], XtNminHeight, 150);
    XtSetArg(arglist[2], XtNheight, 150);
    XtSetArg(arglist[3], XtNwidth, 225);
    XtSetArg(arglist[4], XtNmaxWidth, 225);
    XtSetArg(arglist[5], XtNmaxHeight, 150);
    XtSetArg(arglist[6], XtNiconName, "grass environment");
    eparams = XtCreateApplicationShell("eparams",
        topLevelShellWidgetClass, arglist, 7);

    XtSetArg(arglist[0], XtNheight, 150);
    XtSetArg(arglist[1], XtNwidth, 225);
    XtSetArg(arglist[2], XtNbackground, pixel("white"));
    efoo = XtCreateManagedWidget("env_window", formWidgetClass,
		eparams, arglist, 3);
    XtOverrideTranslations(efoo, form_trans);

    XtSetArg(arglist[0], XtNheight, 20);
    XtSetArg(arglist[1], XtNwidth, 225);
    XtSetArg(arglist[2], XtNlabel, "GRASS ENVIRONMENT");
    XtSetArg(arglist[3], XtNfont, font1);
    XtSetArg(arglist[4], XtNbackground, pixel("red"));
    XtSetArg(arglist[5], XtNforeground, pixel("yellow"));
    etitl = XtCreateManagedWidget("etitl", labelWidgetClass, efoo,
        arglist, 6);

    XtSetArg(arglist[0], XtNheight, 20);
    XtSetArg(arglist[1], XtNwidth, 112);
    XtSetArg(arglist[2], XtNborderColor, pixel("blue"));
    XtSetArg(arglist[3], XtNfromVert, etitl);
    XtSetArg(arglist[4], XtNbackground, pixel("wheat"));
    XtSetArg(arglist[5], XtNfont, font3);
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    mapset = XtCreateManagedWidget("MAPSET      :", labelWidgetClass,
		efoo, arglist, 7);

    XtSetArg(arglist[4], XtNbackground, pixel("white"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[7], XtNfromHoriz, mapset);
    XtSetArg(arglist[8], XtNstring, m_str);
    XtSetArg(arglist[9], XtNeditType, XawtextEdit);
    XtSetArg(arglist[10], XtNlength, 30);
    XtSetArg(arglist[11], XtNhorizDistance, 0);
    XtSetArg(arglist[12], XtNhighlightThickness, 15);
    XtSetArg(arglist[13], XtNleftMargin, 6);
    XtSetArg(arglist[14], XtNinsertPosition, strlen(m_str));
    XtSetArg(arglist[15], XtNcursor, cur1);
    mt = XtCreateManagedWidget("", asciiStringWidgetClass, efoo,
        arglist, 16);

    XtSetArg(arglist[4], XtNbackground, pixel("wheat"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[3], XtNfromVert, mapset);
    location = XtCreateManagedWidget("LOCATION   :",
        labelWidgetClass, efoo, arglist, 7);

    XtSetArg(arglist[4], XtNbackground, pixel("white"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[7], XtNfromHoriz, location);
    XtSetArg(arglist[8], XtNstring, l_str);
    XtSetArg(arglist[14], XtNinsertPosition, strlen(l_str));
    lt = XtCreateManagedWidget("lt", asciiStringWidgetClass, efoo,
        arglist, 16);

    XtSetArg(arglist[4], XtNbackground, pixel("wheat"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[3], XtNfromVert, location);
    gisdbase = XtCreateManagedWidget("GISDBASE   :", labelWidgetClass,
		efoo, arglist, 7);

    XtSetArg(arglist[4], XtNbackground, pixel("white"));
    XtSetArg(arglist[6], XtNforeground, pixel("black"));
    XtSetArg(arglist[7], XtNfromHoriz, gisdbase);
    XtSetArg(arglist[8], XtNstring, g_str);
    XtSetArg(arglist[14], XtNinsertPosition, strlen(g_str));
    gt = XtCreateManagedWidget("gt", asciiStringWidgetClass, efoo,
        arglist, 16);

    XtSetArg(arglist[0], XtNheight, 20);
    XtSetArg(arglist[1], XtNwidth, 225);
    XtSetArg(arglist[2], XtNfromVert, gisdbase);
    XtSetArg(arglist[3], XtNbackground, pixel("blue"));
    XtSetArg(arglist[4], XtNforeground, pixel("yellow"));
    XtSetArg(arglist[5], XtNhighlightThickness, 15);
    XtSetArg(arglist[6], XtNfont, font2);
    XtSetArg(arglist[7], XtNcallback, set_env_callback);
    setenv = XtCreateManagedWidget("SET ENVIRONMENT",
        commandWidgetClass, efoo, arglist, 8);

    XtRealizeWidget(eparams);
    env_win_flag = 1;
}


void set_environment(widget, event, params, nparams)
Widget widget;
XEvent *event;
char **params;
int *nparams;
{
    G_setenv("MAPSET", m_str);
    G_setenv("LOCATION_NAME", l_str);
    G_setenv("GISDBASE", g_str);

    XtDestroyWidget(XtParent(efoo));
    env_win_flag = 0;
}


read_env_info(m, l, g)
char *m, *l, *g;
{
    char *pm;

    pm = G_getenv("MAPSET");
    strcpy(m, pm);
    pm = G_getenv("LOCATION_NAME");
    strcpy(l, pm);
    pm = G_getenv("GISDBASE");
    strcpy(g, pm);
}
