#include <stdio.h>
#include <string.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/SelectioB.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Interact.h>
#include <Browser.h>
#include <xgbitmaps.h>
#include "Matrix.h"
#define INPUT           100
#define OUTPUT  101
#define UNITS           102
#define DISTANCES       103

Widget theCommand;
Widget theContainer;
Widget          inputText;
Widget          theDialog;
Widget          mainShell;
XtAppContext    appContext;
Display        *display;
short           widths[1] = {12};
char           *options[] = {"meters", "kilometers", "feet", "miles"};
char           *cmd = "r.buffer";
char           *in_p;
char           *out_p;
char           *distance_p;
char           *unit_p;
char          **values;
Widget          shell, buffer_dialog;
Widget          Create_Buffer_Dialog(), Create_Input_Cell(), Create_Output_Cell();
Widget          Create_Unit_Cell(), Create_Assign_Cell();
void            DoBufferDialog(), CallFile(), CallUnit(), CallInputOutput(), UpdateCommand();
void            CallOk(), CallCancel(), CallReset(), CallAccept();
char           *namestring = "helvb12", *zonestring = "helvb14";
Widget          inputField;


static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS Buffer"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

char           *
XbaeMatrixGetColumn(tb, column, rows, columns, separator)
    Widget          tb;
    int             column;
    int             rows;
    int             columns;
    int             separator;
{
    char          **rowtext;
    char           *text;
    Arg             targs[2];
    int             element;
    int             bytes = 0;
    int             i;

    rowtext = (char **) XtCalloc(rows, sizeof(char *));
    bzero((char *) rowtext, rows * sizeof(char *));

    for (i = 0; i < rows; i++) {
        rowtext[i] = XbaeMatrixGetCell(tb, i, column);
        bytes += strlen(rowtext[i]) + 2;
    }
    text = XtMalloc(bytes);
    for (i = 0; i < rows; i++) {
        if (i == 0)
            strcpy(text, rowtext[i]);
        else
            strcat(text, rowtext[i]);
        if ((i != (columns - 1) || columns == 1) &&
            i != (rows - 1))
            if (separator == '\0')
                strcat(text, " ");
            else {
                char            sep[1];

                sprintf(sep, "%c", separator);
                strcat(text, sep);
            }
    }
    XtFree(rowtext);

    return text;
}

main(argc, argv)
    int             argc;
    char           *argv[];
{
    Widget          area, dialog;
    Arg             al[10];
    int             ac;

    out_p = (char *) malloc(300 * sizeof(char));
    in_p = (char *) malloc(300 * sizeof(char));
    distance_p = (char *) malloc(500 * sizeof(char));
    unit_p = (char *) malloc(300 * sizeof(char));
    strcpy(out_p, "output=");
    strcpy(in_p, "inp=");
    strcpy(distance_p, "distances=");
    strcpy(unit_p, "units=");
    G_gisinit(argv[0]);
    mainShell = shell = XtAppInitialize(&appContext, "XGrass",
					initTable, XtNumber(initTable),
					&argc, argv, NULL, NULL, 0);

    display = XtDisplay(shell);

    dialog = Create_Buffer_Dialog(shell);

    XtRealizeWidget(shell);
    XtAppMainLoop(appContext);
}

void 
DoBufferDialog(w, dialog, ca)
    Widget          w, dialog;
    XtPointer       ca;
{
    XtManageChild(dialog);
}

Widget 
Create_Buffer_Dialog(parent)
    Widget          parent;
{
    Widget          global_board;
    Widget          board_1, io_frame, input_board, output_board;
    Widget          board_2, unit_board, assign_board;
    Arg             al[10];
    int             ac;

    ac = 0;
    XtSetArg(al[ac], XmNautoUnmanage, FALSE);
    ac++;
    theDialog = buffer_dialog = XgCreateInteractor(parent, "Perform Buffering Operation", al, ac);
    XmRemoveTabGroup(buffer_dialog);

    XtAddCallback(buffer_dialog, XmNokCallback, CallOk, NULL);
    XtAddCallback(buffer_dialog, XmNcancelCallback, CallCancel, NULL);

    theContainer = XtVaCreateManagedWidget("xgbuffer_container",xmRowColumnWidgetClass,buffer_dialog,
	XmNorientation,XmVERTICAL,
	XmNpacking,XmPACK_TIGHT,
	NULL);

    global_board = XmCreateRowColumn(theContainer, "global_board", NULL, 0);

    io_frame = XmCreateFrame(global_board, "io_frame", NULL, 0);
    XtManageChild(io_frame);
    ac = 0;
    XtSetArg(al[ac], XmNpacking, XmPACK_COLUMN);
    ac++;
    board_1 = XmCreateRowColumn(io_frame, "board_1", al, ac);
    XtManageChild(board_1);
    input_board = Create_Input_Cell(board_1);
    output_board = Create_Output_Cell(board_1);

    ac = 0;
    XtSetArg(al[ac], XmNorientation, XmHORIZONTAL);
    ac++;
    board_2 = XmCreateRowColumn(global_board, "board_2", al, ac);
    XtManageChild(board_2);
    unit_board = Create_Unit_Cell(board_2);
    assign_board = Create_Assign_Cell(board_2);

    theCommand = XtVaCreateManagedWidget("xgbuffer_command",xmTextFieldWidgetClass,theContainer,
	XmNvalue,"r.buffer input= output= distances= units=",
	NULL);

    XtManageChild(theDialog);
    XtManageChild(global_board);
    return (buffer_dialog);
}

Widget 
Create_Assign_Cell(parent)
    Widget          parent;
{
    Widget          assignRC, labelRC, znLB, disLB, table, optionRC, acceptB,
                    resetB;
    char          **headings = (char **) XtCalloc(60, sizeof(char *));
    Display        *dpy = XtDisplay(shell);
    char            name[12];
    Arg             al[10];
    int             ac, i, scr = DefaultScreen(dpy);

    assignRC = XmCreateRowColumn(parent, "assignRC", NULL, 0);
    XtManageChild(assignRC);

    ac = 0;
    XtSetArg(al[ac], XmNpacking, XmPACK_NONE);
    ac++;
    labelRC = XmCreateRowColumn(assignRC, "labelRC", al, ac);
    XtManageChild(labelRC);

    ac = 0;
    XtSetArg(al[ac], XmNlabelString,
             XmStringCreate("zone", XmSTRING_DEFAULT_CHARSET));
    ac++;
    znLB = XmCreateLabel(labelRC, "znLB", al, ac);
    XtManageChild(znLB);

    ac = 0;
    XtSetArg(al[ac], XmNlabelString,
             XmStringCreate("distances", XmSTRING_DEFAULT_CHARSET));
    ac++;
    XtSetArg(al[ac], XmNx, 80);
    ac++;
    disLB = XmCreateLabel(labelRC, "disLB", al, ac);
    XtManageChild(disLB);

    ac = 0;
    for (i = 0; i < 60; i++) {
        sprintf(name, "%3d", i + 1);
        headings[i] = XtNewString(name);
    }
    XtSetArg(al[ac], XmNrowLabels, headings);
    ac++;
    XtSetArg(al[ac], XmNrows, 60);
    ac++;
    XtSetArg(al[ac], XmNmarginWidth, 2);
    ac++;
    XtSetArg(al[ac], XmNcolumns, 1);
    ac++;
    XtSetArg(al[ac], XmNvisibleRows, 4);
    ac++;
    XtSetArg(al[ac], XmNcolumnWidths, widths);
    ac++;
    XtSetArg(al[ac], XmNtraversalOn, True);
    ac++;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;

    table = XtCreateManagedWidget("table", xbaeMatrixWidgetClass, assignRC, al, ac);

    values = (char **) XtMalloc(60 * sizeof(char *));
    for (i = 0; i < 60; i++) {
        values[i] = XtMalloc(10);
        sprintf(values[i], "0");
        XbaeMatrixSetCell(table, i, 0, "0");
    }

    ac = 0;
    XtSetArg(al[ac], XmNorientation, XmHORIZONTAL);
    ac++;
    optionRC = XmCreateForm(assignRC, "optionRC", al, ac);
    XtManageChild(optionRC);

    ac = 0;
    XtSetArg(al[ac], XmNlabelString,
             XmStringCreate("Accept", XmSTRING_DEFAULT_CHARSET));
    ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_POSITION);
    ac++;
    XtSetArg(al[ac], XmNrightPosition, 49);
    ac++;
    acceptB = XmCreatePushButton(optionRC, "acceptB", al, ac);
    XmAddTabGroup(acceptB);
    XtManageChild(acceptB);
    XtAddCallback(acceptB, XmNactivateCallback, CallAccept, table);

    ac = 0;
    XtSetArg(al[ac], XmNlabelString,
             XmStringCreate("Reset", XmSTRING_DEFAULT_CHARSET));
    ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET);
    ac++;
    XtSetArg(al[ac], XmNleftWidget, acceptB);
    ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
    ac++;
    resetB = XmCreatePushButton(optionRC, "resetB", al, ac);
    XmAddTabGroup(resetB);
    XtManageChild(resetB);
    XtAddCallback(resetB, XmNactivateCallback, CallReset, table);

    return (assignRC);
}

Widget 
Create_Unit_Cell(parent)
    Widget          parent;
{
    Widget          frame, rowcol, browser, unit_label;
    WidgetList      buttons;
    Arg             al[10];
    int             ac, i;

    ac = 0;
    XtSetArg(al[ac], XmNspacing, 7);
    ac++;
    rowcol = XtCreateManagedWidget("rowcol",
                                   xmRowColumnWidgetClass, parent, al, ac);

    ac = 0;
    XtSetArg(al[ac], XmNlabelString,
             XmStringCreate("unit", XmSTRING_DEFAULT_CHARSET));
    ac++;
    unit_label = XtCreateManagedWidget("unit_label",
                                       xmLabelWidgetClass, rowcol, al, ac);

    frame = XtCreateManagedWidget("unitframe",
                                  xmFrameWidgetClass, rowcol, NULL, 0);

    buttons = (WidgetList) XtMalloc(4 * sizeof(Widget));
    ac = 0;
    XtSetArg(al[ac], XmNspacing, 14);
    ac++;
    browser = XmCreateRadioBox(frame, "browser", al, ac);
    for (i = 0; i < 4; i++) {
        ac = 0;
        XtSetArg(al[ac], XmNlabelString,
                 XmStringCreate(options[i], XmSTRING_DEFAULT_CHARSET));
        ac++;
        buttons[i] = XtCreateManagedWidget(options[i],
                                xmToggleButtonWidgetClass, browser, al, ac);
        XtAddCallback(buttons[i], XmNarmCallback, CallUnit, i);
        XmAddTabGroup(buttons[i]);
    }
    XtManageChild(browser);
    return (rowcol);
}

Widget 
Create_Input_Cell(parent)
    Widget          parent;
{
    Widget          input_board, input_label, input_text, select_B;
    Arg             al[10];
    int             ac;
    Pixmap          map;
    Pixel           fore, back;

    input_board = XtCreateManagedWidget("input_board",
                                        xmFormWidgetClass, parent, NULL, 0);

    input_label = XtCreateManagedWidget("input_label",
                                  xmLabelWidgetClass, input_board, NULL, 0);

    select_B = XtCreateManagedWidget("select_B",
                             xmPushButtonWidgetClass, input_board, NULL, 0);
    XmAddTabGroup(select_B);
    XtAddCallback(select_B, XmNactivateCallback, CallFile, "raster file");

    ac = 0;
    XtSetArg(al[ac], XmNforeground, &fore);
    ac++;
    XtSetArg(al[ac], XmNbackground, &back);
    ac++;
    XtGetValues(select_B, al, ac);
    map = XCreatePixmapFromBitmapData(display,
                                      DefaultRootWindow(display),
                                      raster_bm_bits,
                                      raster_bm_width,
                                      raster_bm_height,
                                      fore,
                                      back,
                                 DefaultDepthOfScreen(XtScreen(mainShell)));

    if (map == XmUNSPECIFIED_PIXMAP) {
        fprintf(stderr, "Couldn't find pixmap\n");
        XFlush(display);
        exit(1);
    }
    /* setup a input text */
    input_text = inputText = XtCreateManagedWidget("input_text",
                              xmTextFieldWidgetClass, input_board, NULL, 0);
    XtAddCallback(input_text, XmNactivateCallback, CallInputOutput, INPUT);
    XtAddCallback(input_text, XmNlosingFocusCallback, CallInputOutput, INPUT);
    XmAddTabGroup(input_text);

    ac = 0;
    XtSetArg(al[ac], XmNlabelString,
             XmStringCreate("Input map:", XmSTRING_DEFAULT_CHARSET));
    ac++;
    XtSetArg(al[ac], XmNwidth, 100);
    ac++;
    XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
    ac++;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM);
    ac++;
    XtSetValues(input_label, al, ac);

    ac = 0;
    XtSetArg(al[ac], XmNlabelType, XmPIXMAP);
    ac++;
    XtSetArg(al[ac], XmNlabelPixmap, map);
    ac++;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET);
    ac++;
    XtSetArg(al[ac], XmNleftWidget, input_label);
    ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM);
    ac++;
    XtSetValues(select_B, al, ac);

    ac = 0;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET);
    ac++;
    XtSetArg(al[ac], XmNleftWidget, select_B);
    ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM);
    ac++;
    XtSetValues(input_text, al, ac);

    return (input_board);
}

Widget 
Create_Output_Cell(parent)
    Widget          parent;
{
    Widget          output_board, output_label, output_text;
    Arg             al[10];
    int             ac;

    output_board = XtCreateManagedWidget("output_board",
                                         xmFormWidgetClass, parent, NULL, 0);

    output_label = XtCreateManagedWidget("output_label",
                                 xmLabelWidgetClass, output_board, NULL, 0);

    output_text = XtCreateManagedWidget("output_text",
                                  xmTextWidgetClass, output_board, NULL, 0);
    XtAddCallback(output_text, XmNactivateCallback, CallInputOutput, OUTPUT);
    XtAddCallback(output_text, XmNlosingFocusCallback, CallInputOutput, OUTPUT);
    XmAddTabGroup(output_text);

    ac = 0;
    XtSetArg(al[ac], XmNlabelString,
             XmStringCreate("Output map:", XmSTRING_DEFAULT_CHARSET));
    ac++;
    XtSetArg(al[ac], XmNwidth, 100);
    ac++;
    XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
    ac++;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM);
    ac++;
    XtSetValues(output_label, al, ac);

    ac = 0;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET);
    ac++;
    XtSetArg(al[ac], XmNleftWidget, output_label);
    ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM);
    ac++;
    XtSetValues(output_text, al, ac);

    return (output_board);
}

void
BrowserOkCallback(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    char           *s;
    XmString        result;
    s = (char *) malloc(1024 * sizeof(char));
    XtVaGetValues(w, XmNresultString, &result, NULL);
    XmStringGetLtoR(result, XmSTRING_DEFAULT_CHARSET, &s);
    XmTextFieldSetString(inputText, s);
    UpdateCommand(s, INPUT);
}

void
BrowserCancelCallback(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
}

void 
CallFile(w, str, call_data)
    Widget          w;
    char           *str;
    XmAnyCallbackStruct *call_data;
{
    Widget          files;
    Arg             al[10];
    int             ac;

    ac = 0;
    XtSetArg(al[ac], XmNselMode, XG_SINGLE_SELECT);
    ac++;
    XtSetArg(al[ac], XmNbrowseMode, XG_RASTER);
    ac++;
    XtSetArg(al[ac], XmNnumLists, 1);
    ac++;
    files = XgCreateBrowserDialog(w, "Select a Raster Map", al, ac);
    XtAddCallback(files, XmNokCallback, BrowserOkCallback, NULL);
    XtAddCallback(files, XmNcancelCallback, BrowserCancelCallback, NULL);
    XtManageChild(files);
}

void 
CallUnit(w, op, call_data)
    Widget          w;
    int             op;
    caddr_t         call_data;
{
    char            temp[126];

    switch (op) {
    case 0:
        sprintf(temp, "meters");
        break;
    case 1:
        sprintf(temp, "kilometers");
        break;
    case 2:
        sprintf(temp, "feet");
        break;
    case 3:
        sprintf(temp, "miles");
        break;
    }
    UpdateCommand(temp, UNITS);
}

void 
CallInputOutput(w, client, data)
    Widget          w;
    int             client;
    caddr_t         data;
{
    char            temp[256];

    sprintf(temp, "%s", XmTextGetString(w));
    UpdateCommand(temp, client);
}

void 
UpdateCommand(comd, option)
    char           *comd;
    int             option;
{
    Arg             al[10];
    int             ac, size;
    char           *cmd_string;

    cmd_string = (char *) malloc(556 * sizeof(char));
    size = strlen(comd);
    switch (option) {
    case INPUT:
        sprintf(in_p, "input=%s", comd);
        break;
    case OUTPUT:
        sprintf(out_p, "output=%s", comd);
        break;
    case UNITS:
        sprintf(unit_p, "units=%s", comd);
        break;
    case DISTANCES:
        sprintf(distance_p, "distances=%s", comd);
        break;
    }
    sprintf(cmd_string, "%s %s", cmd, in_p);
    sprintf(cmd_string, "%s %s", cmd_string, out_p);
    sprintf(cmd_string, "%s %s", cmd_string, distance_p);
    sprintf(cmd_string, "%s %s", cmd_string, unit_p);
    XmTextFieldSetString(theCommand,cmd_string);
}

void 
CallOk(w, client, call_data)
    Widget          w;
    caddr_t         client, call_data;
{
    int             err = 0;
    char            command[512];
    strcpy(command, XmTextFieldGetString(theCommand));
    XgSystem(theDialog, command, True, &err, 1);

    XgSetCommandString(display, XgGetMenuWindow(display), command);
    XFlush(display);
    exit(0);
}

void 
CallCancel(w, client, call_data)
    Widget          w;
    caddr_t         client, call_data;
{
    XFlush(display);
    exit(0);
}

void 
CallAccept(w, tb, call_data)
    Widget          w, tb;
    caddr_t         call_data;
{
    int             i, flag = 0, size = 0;
    char           *dists, *tok;
    char            temp[660], buf[11];

    dists = XbaeMatrixGetColumn(tb, 0, 60, 1, 0);
    sprintf(temp, "none");
    strtok(dists, " ");
    if (strcmp(dists, "0")) {
        flag = 1;
        sprintf(temp, "%d", atoi(dists));
    }
    for (i = 1; i < 60; i++) {
        tok = (char *) strtok(NULL, " ");
        if (strcmp(tok, "0")) {
            if (flag == 1) {
                sprintf(buf, ",%d", atoi(tok));
                strcat(temp, buf);
            } else {
                flag = 1;
                sprintf(temp, "%d", atoi(tok));
            }
        }
    }
    if (!strcmp(temp, "none"))
        UpdateCommand("", DISTANCES);
    else
        UpdateCommand(temp, DISTANCES);
}

void 
CallReset(w, tb, call_data)
    Widget          w, tb;
    caddr_t         call_data;
{
    int             i;

    for (i = 0; i < 60; i++) {
        sprintf(values[i], "0");
        XbaeMatrixSetCell(tb, i, 0, "0");
    }
    UpdateCommand("", DISTANCES);
}
