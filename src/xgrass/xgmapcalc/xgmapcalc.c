static char     rcsid[] = "@(#)XGRASS $Id$";
/*
 * File: xgmapcalc.c
 * 
 * Desc: Top level interface program to r.mapcalc
 * 
 * Auth: Eric W. Sink
 * 
 * Date: 15 April 1992
 * 
 * Modification History:
 * 
 * 
 */
#include "xgrass_lib.h"
#include "Interact.h"
#include "Browser.h"
#include "gis.h"

Display        *display;
Widget          mainshell;
Widget          xgi;

Widget          theForm;
Widget          fieldsContainer;
Widget          buttonsContainer;
Widget          browserContainer;
Widget          fieldsFrame;
Widget          browserFrame;

Widget          resultLabel;
Widget          exprLabel;
Widget          resultField;
Widget          exprField;

Widget          theBrowser;

Widget          op_add;
Widget          op_sub;
Widget          op_mult;
Widget          op_div;
Widget          op_mod;
Widget          op_eq;
Widget          op_ne;
Widget          op_gt;
Widget          op_ge;
Widget          op_lt;
Widget          op_le;
Widget          op_and;
Widget          op_or;
Widget          op_abs;
Widget          op_atan;
Widget          op_cos;
Widget          op_exp;
Widget          op_exp2;
Widget          op_float;
Widget          op_if;
Widget          op_if2;
Widget          op_if3;
Widget          op_if4;
Widget          op_int;
Widget          op_log;
Widget          op_log2;
Widget          op_max;
Widget          op_min;
Widget          op_round;
Widget          op_sin;
Widget          op_sqrt;
Widget          op_tan;
Widget          op_lparen;
Widget          op_rparen;
Widget          op_hash;
Widget          op_comma;
Widget          op_assign;
Widget          op_eval;
Widget          op_lbrack;
Widget          op_rbrack;
Widget          op_clear;

XtAppContext    appContext;

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS Mapcalc"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

static          Boolean
isFunction(w)
    Widget          w;
{
    if (w == op_add)
        return False;
    if (w == op_sub)
        return False;
    if (w == op_mult)
        return False;
    if (w == op_div)
        return False;
    if (w == op_mod)
        return False;
    if (w == op_gt)
        return False;
    if (w == op_eq)
        return False;
    if (w == op_ne)
        return False;
    if (w == op_ge)
        return False;
    if (w == op_lt)
        return False;
    if (w == op_le)
        return False;
    if (w == op_and)
        return False;
    if (w == op_or)
        return False;
    if (w == op_lparen)
        return False;
    if (w == op_rparen)
        return False;
    if (w == op_hash)
        return False;
    if (w == op_comma)
        return False;
    if (w == op_assign)
        return False;
    if (w == op_lbrack)
        return False;
    if (w == op_rbrack)
        return False;
    if (w == op_clear)
        return False;
    return True;
}

static void
#ifdef _NO_PROTO
CB_insert_map(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
#else
CB_insert_map(Widget w, XtPointer data, XtPointer cbs)
#endif
{
    /* We now insert the name of a map into the expr text field */
    XmTextPosition  ins;
    char           *s;
    XmString        result;
    char            map[512];

    XtVaGetValues(w, XmNresultString, &result, NULL);
    XmStringGetLtoR(result, XmSTRING_DEFAULT_CHARSET, &s);
    sprintf(map, "\"%s\"", s);

    if (s) {
        ins = XmTextGetInsertionPosition(exprField);

        XmTextInsert(exprField, ins, map);
        XmTextSetInsertionPosition(exprField, ins + strlen(map));
    }
}

static void
#ifdef _NO_PROTO
CB_op_clear(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
#else
CB_op_clear(Widget w, XtPointer data, XtPointer cbs)
#endif
{
    XmTextSetString(exprField, "");
}

static void
#ifdef _NO_PROTO
CB_op_generic(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
#else
CB_op_generic(Widget w, XtPointer data, XtPointer cbs)
#endif
{
    char           *sel;
    XmString        label;
    char           *name;
    XmTextPosition  left, right;
    XmTextPosition  ins;
    char            func[512];

    ins = XmTextGetInsertionPosition(exprField);
    sel = XmTextGetSelection(exprField);
    if (sel) {
        XmTextGetSelectionPosition(exprField, &left, &right);
    }
    XtVaGetValues(w, XmNlabelString, &label, NULL);
    XmStringGetLtoR(label, XmSTRING_DEFAULT_CHARSET, &name);
    if (sel) {
        /*
         * If this is a function, we now insert the name of the func such
         * that it applies to the selection.  If it is an op, we simply
         * insert the op after the selection
         */
        if (isFunction(w)) {
            sprintf(func, "%s(%s)", name, sel);
            XmTextReplace(exprField, left, right, func);
            XtFree(sel);
        } else {
            XmTextInsert(exprField, ins, name);
            XmTextSetInsertionPosition(exprField, ins + strlen(name));
        }
    } else {
        /*
         * If this is a function, we insert the function name, with its
         * parens, and leave the insertion point between the parens.  If this
         * is an op, we insert the op, and leave the insertion point right
         * after the op.
         */
        if (isFunction(w)) {
            XmTextInsert(exprField, ins, name);
            XmTextInsert(exprField, ins + strlen(name), "()");
            XmTextSetInsertionPosition(exprField, ins + strlen(name) + 1);
        } else {
            XmTextInsert(exprField, ins, name);
            XmTextSetInsertionPosition(exprField, ins + strlen(name));
        }
    }
}

static void
#ifdef _NO_PROTO
XgmapcalcOk(w, data)
    Widget          w;
    XtPointer       data;
#else
XgmapcalcOk(Widget w, XtPointer data)
#endif
{
    int             i;
    int             err;
    char           *s;
    char            command[1024];

    strcpy(command, "r.mapcalc '");

    strcat(command, XmTextFieldGetString(resultField));
    strcat(command, "=");
    strcat(command, XmTextGetString(exprField));
    strcat(command, "'");

    printf("Executing %s\n", command);
    XgSystem(xgi, command, True, &err, 1);

}

static void
#ifdef _NO_PROTO
XgmapcalcCancel(w, data)
    Widget          w;
    XtPointer       data;
#else
XgmapcalcCancel(Widget w, XtPointer data)
#endif
{
    XFlush(display);
    exit(1);
}

static int
#ifdef _NO_PROTO
CreateLayout(w)
    Widget          w;
#else
CreateLayout(Widget w)
#endif
{
    Arg             al[16];
    int             ac = 0;
    char            mapset[512];

    XtSetArg(al[ac], XmNenableWorkAreaStretch, True);
    ac++;
    XtSetArg(al[ac], XmNokLabelString, XmStringCreateSimple("Execute"));
    ac++;
    XtSetArg(al[ac], XmNcancelLabelString, XmStringCreateSimple("Done"));
    ac++;
    xgi = XgCreateInteractor(w, "Raster map layer data calculator", al, ac);
    XtManageChild(xgi);

    XtAddCallback(xgi, XmNokCallback, XgmapcalcOk, xgi);
    XtAddCallback(xgi, XmNcancelCallback, XgmapcalcCancel, xgi);

    theForm = XtVaCreateManagedWidget("xgmapcalc_form", xmFormWidgetClass, xgi,
                                      NULL);

    fieldsFrame = XtVaCreateManagedWidget("xgmapcalc_fields_frame", xmFrameWidgetClass, theForm,
                                          XmNleftAttachment, XmATTACH_FORM,
                                          XmNrightAttachment, XmATTACH_FORM,
                                          XmNtopAttachment, XmATTACH_FORM,
                                          /*XmNbottomAttachment, XmATTACH_FORM,*/
                                          NULL);

    buttonsContainer = XtVaCreateManagedWidget("xgmapcalc_buttons_container", xmRowColumnWidgetClass, theForm,
                                        XmNtopAttachment, XmATTACH_WIDGET,
                                               XmNtopWidget, fieldsFrame,
                                            XmNleftAttachment, XmATTACH_FORM,
                                         /*XmNleftAttachment, XmATTACH_WIDGET,*/
                                               /*XmNleftWidget, fieldsFrame,*/
                                            /*XmNtopAttachment, XmATTACH_FORM,*/
                                      XmNentryAlignment, XmALIGNMENT_CENTER,
                                               XmNpacking, XmPACK_COLUMN,
                                               XmNorientation, XmHORIZONTAL,
                                               XmNnumColumns, 8,
                                               NULL);

    browserFrame = XtVaCreateManagedWidget("xgmapcalc_browser_frame", xmFrameWidgetClass, theForm,
                                         XmNleftAttachment, XmATTACH_WIDGET,
                                           XmNleftWidget, buttonsContainer,
                                           XmNtopAttachment, XmATTACH_WIDGET,
                                           XmNtopWidget, fieldsFrame,
                                         XmNbottomAttachment, XmATTACH_FORM,
                                           NULL);

    fieldsContainer = XtVaCreateManagedWidget("xgmapcalc_fields_container", xmRowColumnWidgetClass, fieldsFrame,
                                              XmNpacking, XmPACK_TIGHT,
                                              XmNorientation, XmVERTICAL,
                                              NULL);

    browserContainer = XtVaCreateManagedWidget("xgmapcalc_browser_container", xmRowColumnWidgetClass, browserFrame,
                                               XmNpacking, XmPACK_TIGHT,
                                               XmNorientation, XmVERTICAL,
                                               NULL);

    resultLabel = XtVaCreateManagedWidget("xgmapcalc_result_label", xmLabelWidgetClass, fieldsContainer,
                        XmNlabelString, XmStringCreateSimple("Result map:"),
                                          XmNtraversalOn, False,
                                          NULL);

    resultField = XtVaCreateManagedWidget("xgmapcalc_result_field", xmTextFieldWidgetClass, fieldsContainer,
                                          XmNvalue, "mapcalc.out",
                                          NULL);

    exprLabel = XtVaCreateManagedWidget("xgmapcalc_expr_label", xmLabelWidgetClass, fieldsContainer,
                        XmNlabelString, XmStringCreateSimple("Expression:"),
                                        XmNtraversalOn, False,
                                        NULL);

    ac = 0;
    XtSetArg(al[ac], XmNeditMode, XmMULTI_LINE_EDIT); ac++;
    XtSetArg(al[ac], XmNrows, 4); ac++;
    XtSetArg(al[ac], XmNwordWrap, True); ac++;
    XtSetArg(al[ac], XmNeditable, True); ac++;
    exprField = XmCreateScrolledText(fieldsContainer, "xgmapcalc_expr_field", 
                                     al, ac);
    XtManageChild(exprField);
    XtManageChild(XtParent(exprField));

    op_add = XtVaCreateManagedWidget("xgmapcalc_op_add", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple("+"),
                                     NULL);
    XtAddCallback(op_add, XmNactivateCallback, CB_op_generic, NULL);

    op_sub = XtVaCreateManagedWidget("xgmapcalc_op_sub", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple("-"),
                                     NULL);
    XtAddCallback(op_sub, XmNactivateCallback, CB_op_generic, NULL);

    op_mult = XtVaCreateManagedWidget("xgmapcalc_op_mult", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple("*"),
                                      NULL);
    XtAddCallback(op_mult, XmNactivateCallback, CB_op_generic, NULL);

    op_div = XtVaCreateManagedWidget("xgmapcalc_op_div", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple("/"),
                                     NULL);
    XtAddCallback(op_div, XmNactivateCallback, CB_op_generic, NULL);

    op_mod = XtVaCreateManagedWidget("xgmapcalc_op_mod", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple("%"),
                                     NULL);
    XtAddCallback(op_mod, XmNactivateCallback, CB_op_generic, NULL);

    op_eq = XtVaCreateManagedWidget("xgmapcalc_op_eq", xmPushButtonWidgetClass, buttonsContainer,
                                 XmNlabelString, XmStringCreateSimple("=="),
                                    NULL);
    XtAddCallback(op_eq, XmNactivateCallback, CB_op_generic, NULL);

    op_ne = XtVaCreateManagedWidget("xgmapcalc_op_ne", xmPushButtonWidgetClass, buttonsContainer,
                                 XmNlabelString, XmStringCreateSimple("!="),
                                    NULL);
    XtAddCallback(op_ne, XmNactivateCallback, CB_op_generic, NULL);

    op_gt = XtVaCreateManagedWidget("xgmapcalc_op_gt", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple(">"),
                                    NULL);
    XtAddCallback(op_gt, XmNactivateCallback, CB_op_generic, NULL);

    op_ge = XtVaCreateManagedWidget("xgmapcalc_op_ge", xmPushButtonWidgetClass, buttonsContainer,
                                 XmNlabelString, XmStringCreateSimple(">="),
                                    NULL);
    XtAddCallback(op_ge, XmNactivateCallback, CB_op_generic, NULL);

    op_lt = XtVaCreateManagedWidget("xgmapcalc_op_lt", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple("<"),
                                    NULL);
    XtAddCallback(op_lt, XmNactivateCallback, CB_op_generic, NULL);

    op_le = XtVaCreateManagedWidget("xgmapcalc_op_le", xmPushButtonWidgetClass, buttonsContainer,
                                 XmNlabelString, XmStringCreateSimple("<="),
                                    NULL);
    XtAddCallback(op_le, XmNactivateCallback, CB_op_generic, NULL);

    op_and = XtVaCreateManagedWidget("xgmapcalc_op_and", xmPushButtonWidgetClass, buttonsContainer,
                                 XmNlabelString, XmStringCreateSimple("&&"),
                                     NULL);
    XtAddCallback(op_and, XmNactivateCallback, CB_op_generic, NULL);

    op_or = XtVaCreateManagedWidget("xgmapcalc_op_or", xmPushButtonWidgetClass, buttonsContainer,
                                 XmNlabelString, XmStringCreateSimple("||"),
                                    NULL);
    XtAddCallback(op_or, XmNactivateCallback, CB_op_generic, NULL);

    op_abs = XtVaCreateManagedWidget("xgmapcalc_op_abs", xmPushButtonWidgetClass, buttonsContainer,
                                XmNlabelString, XmStringCreateSimple("abs"),
                                     NULL);
    XtAddCallback(op_abs, XmNactivateCallback, CB_op_generic, NULL);

    op_atan = XtVaCreateManagedWidget("xgmapcalc_op_atan", xmPushButtonWidgetClass, buttonsContainer,
                               XmNlabelString, XmStringCreateSimple("atan"),
                                      NULL);
    XtAddCallback(op_atan, XmNactivateCallback, CB_op_generic, NULL);

    op_cos = XtVaCreateManagedWidget("xgmapcalc_op_cos", xmPushButtonWidgetClass, buttonsContainer,
                                XmNlabelString, XmStringCreateSimple("cos"),
                                     NULL);
    XtAddCallback(op_cos, XmNactivateCallback, CB_op_generic, NULL);

    op_exp = XtVaCreateManagedWidget("xgmapcalc_op_exp", xmPushButtonWidgetClass, buttonsContainer,
                                XmNlabelString, XmStringCreateSimple("exp"),
                                     NULL);
    XtAddCallback(op_exp, XmNactivateCallback, CB_op_generic, NULL);

    op_float = XtVaCreateManagedWidget("xgmapcalc_op_float", xmPushButtonWidgetClass, buttonsContainer,
                              XmNlabelString, XmStringCreateSimple("float"),
                                       NULL);
    XtAddCallback(op_float, XmNactivateCallback, CB_op_generic, NULL);

    op_if = XtVaCreateManagedWidget("xgmapcalc_op_if", xmPushButtonWidgetClass, buttonsContainer,
                                 XmNlabelString, XmStringCreateSimple("if"),
                                    NULL);
    XtAddCallback(op_if, XmNactivateCallback, CB_op_generic, NULL);

    op_int = XtVaCreateManagedWidget("xgmapcalc_op_int", xmPushButtonWidgetClass, buttonsContainer,
                                XmNlabelString, XmStringCreateSimple("int"),
                                     NULL);
    XtAddCallback(op_int, XmNactivateCallback, CB_op_generic, NULL);

    op_log = XtVaCreateManagedWidget("xgmapcalc_op_log", xmPushButtonWidgetClass, buttonsContainer,
                                XmNlabelString, XmStringCreateSimple("log"),
                                     NULL);
    XtAddCallback(op_log, XmNactivateCallback, CB_op_generic, NULL);

    op_max = XtVaCreateManagedWidget("xgmapcalc_op_max", xmPushButtonWidgetClass, buttonsContainer,
                                XmNlabelString, XmStringCreateSimple("max"),
                                     NULL);
    XtAddCallback(op_max, XmNactivateCallback, CB_op_generic, NULL);

    op_min = XtVaCreateManagedWidget("xgmapcalc_op_min", xmPushButtonWidgetClass, buttonsContainer,
                                XmNlabelString, XmStringCreateSimple("min"),
                                     NULL);
    XtAddCallback(op_min, XmNactivateCallback, CB_op_generic, NULL);

    op_round = XtVaCreateManagedWidget("xgmapcalc_op_round", xmPushButtonWidgetClass, buttonsContainer,
                              XmNlabelString, XmStringCreateSimple("round"),
                                       NULL);
    XtAddCallback(op_round, XmNactivateCallback, CB_op_generic, NULL);

    op_sin = XtVaCreateManagedWidget("xgmapcalc_op_sin", xmPushButtonWidgetClass, buttonsContainer,
                                XmNlabelString, XmStringCreateSimple("sin"),
                                     NULL);
    XtAddCallback(op_sin, XmNactivateCallback, CB_op_generic, NULL);

    op_sqrt = XtVaCreateManagedWidget("xgmapcalc_op_sqrt", xmPushButtonWidgetClass, buttonsContainer,
                               XmNlabelString, XmStringCreateSimple("sqrt"),
                                      NULL);
    XtAddCallback(op_sqrt, XmNactivateCallback, CB_op_generic, NULL);

    op_tan = XtVaCreateManagedWidget("xgmapcalc_op_tan", xmPushButtonWidgetClass, buttonsContainer,
                                XmNlabelString, XmStringCreateSimple("tan"),
                                     NULL);
    XtAddCallback(op_tan, XmNactivateCallback, CB_op_generic, NULL);

    op_lparen = XtVaCreateManagedWidget("xgmapcalc_op_lparen", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple("("),
                                        NULL);
    XtAddCallback(op_lparen, XmNactivateCallback, CB_op_generic, NULL);

    op_rparen = XtVaCreateManagedWidget("xgmapcalc_op_rparen", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple(")"),
                                        NULL);
    XtAddCallback(op_rparen, XmNactivateCallback, CB_op_generic, NULL);

    op_hash = XtVaCreateManagedWidget("xgmapcalc_op_hash", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple("#"),
                                      NULL);
    XtAddCallback(op_hash, XmNactivateCallback, CB_op_generic, NULL);

    op_comma = XtVaCreateManagedWidget("xgmapcalc_op_comma", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple(","),
                                       NULL);
    XtAddCallback(op_comma, XmNactivateCallback, CB_op_generic, NULL);

    op_assign = XtVaCreateManagedWidget("xgmapcalc_op_assign", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple("="),
                                        NULL);
    XtAddCallback(op_assign, XmNactivateCallback, CB_op_generic, NULL);

    op_eval = XtVaCreateManagedWidget("xgmapcalc_op_eval", xmPushButtonWidgetClass, buttonsContainer,
                               XmNlabelString, XmStringCreateSimple("eval"),
                                      NULL);
    XtAddCallback(op_eval, XmNactivateCallback, CB_op_generic, NULL);

    op_lbrack = XtVaCreateManagedWidget("xgmapcalc_op_lbrack", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple("["),
                                        NULL);
    XtAddCallback(op_lbrack, XmNactivateCallback, CB_op_generic, NULL);

    op_rbrack = XtVaCreateManagedWidget("xgmapcalc_op_rbrack", xmPushButtonWidgetClass, buttonsContainer,
                                  XmNlabelString, XmStringCreateSimple("]"),
                                        NULL);
    XtAddCallback(op_rbrack, XmNactivateCallback, CB_op_generic, NULL);

    op_clear = XtVaCreateManagedWidget("xgmapcalc_op_clear", xmPushButtonWidgetClass, buttonsContainer,
                                XmNlabelString, XmStringCreateSimple("CLR"),
                                       NULL);
    XtAddCallback(op_clear, XmNactivateCallback, CB_op_clear, NULL);

    if (G_mapset()) {
        strcpy(mapset, G_mapset());
    } else {
        strcpy(mapset, "PERMANENT");
    }
    theBrowser = XtVaCreateManagedWidget("xgmapcalc_browser", browserWidgetClass,
                                         browserContainer,
                            XmNinitialMapset1, XmStringCreateSimple(mapset),
                                         XmNnumLists, 1,
                                         XmNlist1IsStatic, False,
                                         XmNselMode, XG_SINGLE_SELECT,
                                         XmNbrowseMode, XG_RASTER,
                           XmNokLabelString, XmStringCreateSimple("Insert"),
                                         NULL);
    XtAddCallback(theBrowser, XmNokCallback, CB_insert_map, NULL);
    XtUnmanageChild(XgInteractorGetChild(theBrowser, XmINTERACT_HELP_BUTTON));
    XtUnmanageChild(XgInteractorGetChild(theBrowser, XmINTERACT_CANCEL_BUTTON));
}

int
#ifdef _NO_PROTO
main(argc, argv)
    unsigned int    argc;
    char          **argv;
#else
main(unsigned int argc, char **argv)
#endif
{
    Widget          shell;
    Arg             al[16];
    int             ac;

    /* initialize the toolkit  */
    /* and open the display (and a few other things...)  */
    G_gisinit(argv[0]);
    mainshell = shell = XtAppInitialize(&appContext, "XGrass",
					initTable, XtNumber(initTable),
					&argc, argv, NULL, NULL, 0);

    display = XtDisplay(shell);

    CreateLayout(mainshell);

    XtRealizeWidget(shell);
    XtAppMainLoop(appContext);

    return 0;
}
