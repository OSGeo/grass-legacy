static char rcsid[] = "@(#)XGRASS $Id: History.c,v 0.0 1992/05/05 14:56:01 sink Exp sink $";
/*
 * File: History.c
 *
 * Desc: Implementation of History widget
 *
 * Auth: Eric W. Sink
 *
 * Date: 24 Feb 1992
 *
 * Modification History:
 *
 *
 */

#include <HistoryP.h>

#include <X11/StringDefs.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/TextF.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/DrawingA.h>
#include <Xm/Scale.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/DialogS.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/ScrolledW.h>
#include "XmLinux.h"
#include "gis.h"
#include "string.h"

#include <stdio.h>

extern char    *_XgStrDup();
extern void    *_XgMalloc();

/*---------------------------------------------------*/
/* forward declarations                              */
/* */
/* this is a list of all private procedures in this  */
/* module                                            */
/*---------------------------------------------------*/

#ifdef _NO_PROTO

static void     ClassInitialize();
static void     ClassPartInitialize();
static void     Initialize();
static void     Destroy();
static void     DeleteChild();
static Boolean  SetValues();
static void     XgHistoryCallBack();
static void     HistoryDataSetup();
static void     HistoryCreateWidgets();
static void     HistoryAdjustAndAttach();
static void     ValidateAndUpdateEdhist();

#else                           /* _NO_PROTO */

static void     ClassInitialize();
static void     ClassPartInitialize(HistoryWidgetClass xghc);
static void
                Initialize(HistoryWidget request, HistoryWidget new);
static void     Destroy(HistoryWidget xgh);
static void     DeleteChild(Widget w);
static void     HistoryDataSetup(Widget);
static void     HistoryBuildResult(Widget);
static Boolean
SetValues(HistoryWidget current,
          HistoryWidget request,
          HistoryWidget new);
static void
XgHistoryCallBack(Widget wid,
                  XtPointer which_button,
                  XmAnyCallbackStruct * callback);
static void     HistoryCreateWidgets(HistoryWidget xgh);
static void     HistoryAdjustAndAttach(HistoryWidget xgh, Boolean attach);
static void     ValidateAndUpdateEdhist(Widget xgh);

#endif                          /* _NO_PROTO */

/*---------------------------------------------------*/
/* widget resources                                  */
/*---------------------------------------------------*/
static XtResource resources[] =
{
    /* History specific resources */

    {XmNhistory,
        XmCHistory,
        XmRHistoryStructPtr,
        sizeof(struct History *),
        XtOffset(HistoryWidget, history.myHistory),
        XmRImmediate,
        (XtPointer) 0
    },

    /* superclass resource default overrides */

    {XmNautoUnmanage,
        XmCAutoUnmanage,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(HistoryWidget, bulletin_board.auto_unmanage),
        XmRImmediate,
        (XtPointer) TRUE
    },
    {XmNdialogType,
        XmCDialogType,
        XmRDialogType,
        sizeof(unsigned char),
        XtOffset(HistoryWidget, interactor.dialog_type),
        XmRImmediate,
        (XtPointer) XmINTERACT_WORK_AREA_TYPE
    },

};

/* KAB -- I don't think we're done above...yet */
static XmSyntheticResource syn_resources[] =
{
    /* synthetic resources go here KAB */
    {NULL, 0, 0, NULL, NULL}
};

externaldef(xghistoryclassrec)
    HistoryClassRec historyClassRec =
    {
        {                       /* core class record        */
             /* superclass          */ (WidgetClass) & interactorClassRec,
             /* class_name          */ "History",
             /* widget_size         */ sizeof(HistoryRec),
             /* class_initialize    */ ClassInitialize,
             /* class part init     */ ClassPartInitialize,
             /* class_inited        */ FALSE,
             /* initialize          */ Initialize,
             /* initialize hook     */ NULL,
             /* realize             */ _XtInherit,
             /* actions             */ NULL,
             /* num_actions         */ 0,
             /* resources           */ resources,
             /* num_resources       */ XtNumber(resources),
             /* xrm_class           */ NULLQUARK,
             /* compress_motion     */ TRUE,
             /* compress_exposure   */ XtExposeCompressMaximal,
             /* compress crossing   */ FALSE,
             /* visible_interest    */ FALSE,
             /* destroy             */ Destroy,
             /* resize              */ _XtInherit,
             /* expose              */ _XtInherit,
             /* set_values          */ SetValues,
             /* set_values_hook     */ NULL,
             /* set_values_almost   */ _XtInherit,
             /* get_values_hook     */ NULL,
             /* accept_focus        */ NULL,
             /* version             */ XtVersion,
             /* callback_offsets    */ NULL,
             /* tm_table            */ XtInheritTranslations,
             /* query_geometry      */ (XtGeometryHandler) _XtInherit,
             /* display_accelerator */ NULL,
             /* extension           */ NULL,
        },
        {                       /* composite class record   */
             /* geometry manager */ (XtGeometryHandler) _XtInherit,
             /* set changed proc */ _XtInherit,
             /* insert_child     */ _XtInherit,
             /* delete_child     */ DeleteChild,
             /* extension        */ NULL,
        },
        {                       /* constraint class record  */
             /* no additional resources  */ NULL,
             /* num additional resources */ 0,
             /* size of constraint rec   */ 0,
             /* constraint_initialize    */ NULL,
             /* constraint_destroy       */ NULL,
             /* constraint_setvalue      */ NULL,
             /* extension                */ NULL,
        },
        {                       /* manager class record     */
             /* translations                 */ XtInheritTranslations,
             /* get_resources                */ syn_resources,
             /* num_syn_resources            */ XtNumber(syn_resources),
             /* constraint_syn_resources     */ NULL,
             /* num_constraint_syn_resources */ 0,
             /* parent_process               */ XmInheritParentProcess,
             /* extension                    */ NULL,
        },
        {                       /* bulletinBoard class record */
             /* always_install_accelerators */ TRUE,
             /* geo_matrix_create           */ (XmGeoCreateProc) _XtInherit,
             /* focus_moved_proc            */ NULL,
             /* extension                   */ NULL,
        },
        {                       /* interactor class record */
             /* extension */ NULL,
        },
        {                       /* history class record */
             /* extension */ NULL,
        }
    };

externaldef(xghistorywidgetclass) WidgetClass
historyWidgetClass = (WidgetClass) & historyClassRec;

/****************************************************************/
    static void
                    ClassInitialize()
/*****************/
{

    return;
}


/****************************************************************/
static void
#ifdef _NO_PROTO
ClassPartInitialize(xgh)
    HistoryWidgetClass xgh;
#else
ClassPartInitialize(HistoryWidgetClass xgh)
#endif
/****************
 * Class Initialization.  Sets up accelerators and fast subclassing.
 ****************/
{
    /****************/

    _XmFastSubclassInit(xgh, XmHISTORY_BIT);

    return;
}

static void
HistoryNextField(w,data,cbs)
Widget w;
XtPointer data;
XtPointer cbs;
{
    Widget xgh = (Widget) data;
    XmProcessTraversal(xgh, XmTRAVERSE_NEXT_TAB_GROUP);
}

static void
EdhistUpdate(w, data)
    Widget          w;
    XtPointer       data;
{
    Widget          xgh = (Widget) data;

    ValidateAndUpdateEdhist(xgh);
}

static void
HistoryUpdateStruct(w, data)
    Widget          w;
    XtPointer       data;
{
    Widget          xgh = (Widget) data;
    char           *val;

    val = XmTextFieldGetString(H_MapidText(xgh));
    strcpy(H_MyHistory(xgh)->mapid, val);
    val = XmTextFieldGetString(H_TitleText(xgh));
    strcpy(H_MyHistory(xgh)->title, val);
    val = XmTextFieldGetString(H_MapsetText(xgh));
    strcpy(H_MyHistory(xgh)->mapset, val);
    val = XmTextFieldGetString(H_CreatorText(xgh));
    strcpy(H_MyHistory(xgh)->creator, val);
    val = XmTextFieldGetString(H_MaptypeText(xgh));
    strcpy(H_MyHistory(xgh)->maptype, val);
    val = XmTextFieldGetString(H_Datsrc1Text(xgh));
    strcpy(H_MyHistory(xgh)->datsrc_1, val);
    val = XmTextFieldGetString(H_Datsrc2Text(xgh));
    strcpy(H_MyHistory(xgh)->datsrc_2, val);
    val = XmTextFieldGetString(H_KeywrdText(xgh));
    strcpy(H_MyHistory(xgh)->keywrd, val);

    ValidateAndUpdateEdhist(xgh);
}

static void
ValidateAndUpdateEdhist(xgh)
    Widget xgh;
{
    char *val;
    int len;
    char           *line;
    int             i;
    char            buf[RECORD_LEN + 1];
    int             j;
    int             k;

    val = XmTextGetString(H_EdhistText(xgh));
    len = strlen(val);
    i = 0;
    k = 0;
    while (val[i] && (k<MAXEDLINES)) {
        j = 0;
        while (val[i] && val[i] != '\n' && j < (RECORD_LEN - 1) ) {
            buf[j++] = val[i++];
        }
        if ( val[i] && val[i] == '\n' ) {
	    i++;
        } else {
            while (val[i] && val[i] != '\n') i++;
        }
        buf[j] = 0;
        strcpy(H_MyHistory(xgh)->edhist[k++], buf);
    }

    H_MyHistory(xgh)->edlinecnt = k;
    while (k < MAXEDLINES) {
        strcpy(H_MyHistory(xgh)->edhist[k++], "");
    }
}

/****************************************************************/
static void
#ifdef _NO_PROTO
Initialize(request, new)
    HistoryWidget   request;
    HistoryWidget   new;
#else
Initialize(HistoryWidget request,
           HistoryWidget new)
#endif
/****************
 * This routine initializes an instance of the history widget.
 * Instance record fields which are shadow resources for child widgets and
 *   which are of an allocated type are set to NULL after they are used, since
 *   the memory identified by them is not owned by the history widget.
 ****************/
{
    Arg             args[16];
    int             numArgs;
    /****************/

    /*
     * Create all of the new widgets and initialize internal data strux.
     */
    HistoryCreateWidgets(new);

    if (request->manager.navigation_type != XmEXCLUSIVE_TAB_GROUP)
        _XmChangeNavigationType((Widget) new, ((XtIsShell(XtParent(request))
                                       ? XmSTICKY_TAB_GROUP : XmTAB_GROUP)));

    HistoryAdjustAndAttach(new, True);

    return;
}

static void
HistoryEdhistOk(w,data,cbs)
Widget w;
XtPointer data;
XtPointer cbs;
{
  Widget xgr = (Widget) data;
  XtUnmanageChild(H_EdhistDialog(data));
}

static void
HistoryEdhistCancel(w,data,cbs)
Widget w;
XtPointer data;
XtPointer cbs;
{
  Widget xgr = (Widget) data;
  XtUnmanageChild(H_EdhistDialog(data));
}

static void
HistoryEdhistDialog(w,data,cbs)
Widget w;
XtPointer data;
XtPointer cbs;
{
  Widget xgr = (Widget) data;
  XtManageChild(H_EdhistDialog(data));
}

/****************************************************************/
static void
#ifdef _NO_PROTO
HistoryCreateWidgets(xgh)
    HistoryWidget   xgh;
#else
HistoryCreateWidgets(HistoryWidget xgh)
#endif
{
    Widget          form;
    Arg             args[15];
    int             nargs = 0;
    XmString        s;
    Widget          swin;
    Widget          firstItem;
    int             i;

    form = XmCreateForm((Widget) xgh, "xghistory_form", args, nargs);
    XgAddHelpCallback(I_WorkArea(xgh),"xghistory_form");

    nargs = 0;
    XtSetArg(args[nargs], XmNenableWorkAreaStretch, True); nargs++;
    H_EdhistDialog(xgh) = XgCreateInteractorDialog((Widget) xgh,"Edit History",args,nargs);

    H_EdhistForm(xgh) = XmCreateForm(H_EdhistDialog(xgh), "xgedhist_form", args, nargs);
    XtManageChild(H_EdhistForm(xgh));

    H_FieldsContainer(xgh) = XtVaCreateManagedWidget("history_fields_container", xmFormWidgetClass, form,
                                                     NULL);
    H_EdhistButton(xgh) = XtVaCreateManagedWidget("history_edhist_button",
	xmPushButtonWidgetClass,form,
	XmNlabelString,XmStringCreateSimple("Press here to edit history"),
	NULL);

    XtAddCallback(H_EdhistButton(xgh),XmNactivateCallback,HistoryEdhistDialog,xgh);
    H_EdhistLabel(xgh) = XtVaCreateManagedWidget("history_edhist_label",
                                                 xmLabelWidgetClass, H_EdhistForm(xgh),
                                                 XmNtraversalOn, False,
                       XmNlabelString, XmStringCreateSimple("Edit history"),
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNleftAttachment, XmATTACH_FORM,
                            XmNrightAttachment, XmATTACH_FORM,
                                                 NULL);

    H_EdhistString(xgh) = _XgMalloc(MAXEDLINES * (RECORD_LEN + 1) + 1);
    H_EdhistString(xgh)[0] = 0;

    for (i = 0; i < H_MyHistory(xgh)->edlinecnt; i++) {
        strcat(H_EdhistString(xgh), H_MyHistory(xgh)->edhist[i]);
        strcat(H_EdhistString(xgh), "\n");
    }

    nargs = 0;
    XtSetArg(args[nargs], XmNscrollingPolicy, XmAUTOMATIC); nargs++;
    XtSetArg(args[nargs], XmNwidth, 250); nargs++;
    XtSetArg(args[nargs], XmNheight, 175); nargs++;
    swin = XmCreateScrolledWindow(H_EdhistForm(xgh), "history_edit_swin",
        args, nargs);
    XtManageChild(swin);

    nargs = 0;
    XtSetArg(args[nargs], XmNvalue, (H_EdhistString(xgh))); nargs++;
    XtSetArg(args[nargs], XmNeditMode, XmMULTI_LINE_EDIT); nargs++;
    XtSetArg(args[nargs], XmNcolumns, RECORD_LEN); nargs++;
    XtSetArg(args[nargs], XmNrows, MAXEDLINES); nargs++;
    XtSetArg(args[nargs], XmNresizeWidth, False); nargs++;
    XtSetArg(args[nargs], XmNresizeHeight, False); nargs++;
    H_EdhistText(xgh) = XmCreateText(swin, "history_text", args, nargs);
    XtManageChild(H_EdhistText(xgh));
    XtAddCallback(H_EdhistText(xgh), XmNlosingFocusCallback, HistoryUpdateStruct, xgh);
    XtAddCallback(H_EdhistDialog(xgh),XmNokCallback,HistoryEdhistOk,xgh);
    XtAddCallback(H_EdhistDialog(xgh),XmNcancelCallback,HistoryEdhistCancel,xgh);
    XgAddHelpCallBackFromFile(H_EdhistText(xgh),"history_text");

    XtVaSetValues(swin,
	XmNworkWindow, H_EdhistText(xgh),
	XmNtopAttachment, XmATTACH_WIDGET,
	XmNtopWidget, H_EdhistLabel(xgh),
	XmNleftAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNbottomAttachment, XmATTACH_FORM, NULL);

    H_MapidLabel(xgh) = XtVaCreateManagedWidget("history_mapid_label",
                                 xmLabelWidgetClass, H_FieldsContainer(xgh),
                                                XmNtraversalOn, False,
                             XmNlabelString, XmStringCreateSimple("Map id"),
                                                NULL);

    H_MapidText(xgh) = XtVaCreateManagedWidget("history_mapid_text",
                             xmTextFieldWidgetClass, H_FieldsContainer(xgh),
                                        XmNvalue, (H_MyHistory(xgh)->mapid),
                                               XmNeditable, True,
                                               XmNmaxLength, RECORD_LEN,
                                               XmNcolumns, RECORD_LEN,
                                               NULL);
    XtAddCallback(H_MapidText(xgh),XmNactivateCallback,HistoryNextField,xgh);
    XtAddCallback(H_MapidText(xgh), XmNlosingFocusCallback, HistoryUpdateStruct, xgh);

    H_TitleLabel(xgh) = XtVaCreateManagedWidget("history_title_label",
                                 xmLabelWidgetClass, H_FieldsContainer(xgh),
                                                XmNtraversalOn, False,
                              XmNlabelString, XmStringCreateSimple("Title"),
                                                NULL);

    H_TitleText(xgh) = XtVaCreateManagedWidget("history_title_text",
                             xmTextFieldWidgetClass, H_FieldsContainer(xgh),
                                        XmNvalue, (H_MyHistory(xgh)->title),
                                               XmNeditable, True,
                                               XmNmaxLength, RECORD_LEN,
                                               XmNcolumns, RECORD_LEN,
                                               NULL);
    XtAddCallback(H_TitleText(xgh),XmNactivateCallback,HistoryNextField,xgh);
    XtAddCallback(H_TitleText(xgh), XmNlosingFocusCallback, HistoryUpdateStruct, xgh);

    H_MapsetLabel(xgh) = XtVaCreateManagedWidget("history_mapset_label",
                                 xmLabelWidgetClass, H_FieldsContainer(xgh),
                                                 XmNtraversalOn, False,
                            XmNlabelString, XmStringCreateSimple("Project"),
                                                 NULL);

    H_MapsetText(xgh) = XtVaCreateManagedWidget("history_mapset_text",
                             xmTextFieldWidgetClass, H_FieldsContainer(xgh),
                                       XmNvalue, (H_MyHistory(xgh)->mapset),
                                                XmNeditable, True,
                                                XmNmaxLength, RECORD_LEN,
                                                XmNcolumns, RECORD_LEN,
                                                NULL);
    XtAddCallback(H_MapsetText(xgh),XmNactivateCallback,HistoryNextField,xgh);
    XtAddCallback(H_MapsetText(xgh), XmNlosingFocusCallback, HistoryUpdateStruct, xgh);

    H_CreatorLabel(xgh) = XtVaCreateManagedWidget("history_creator_label",
                                 xmLabelWidgetClass, H_FieldsContainer(xgh),
                                                  XmNtraversalOn, False,
                            XmNlabelString, XmStringCreateSimple("Creator"),
                                                  NULL);

    H_CreatorText(xgh) = XtVaCreateManagedWidget("history_creator_text",
                             xmTextFieldWidgetClass, H_FieldsContainer(xgh),
                                      XmNvalue, (H_MyHistory(xgh)->creator),
                                                 XmNeditable, True,
                                                 XmNmaxLength, RECORD_LEN,
                                                 XmNcolumns, RECORD_LEN,
                                                 NULL);
    XtAddCallback(H_CreatorText(xgh),XmNactivateCallback,HistoryNextField,xgh);
    XtAddCallback(H_CreatorText(xgh), XmNlosingFocusCallback, HistoryUpdateStruct, xgh);

    H_MaptypeLabel(xgh) = XtVaCreateManagedWidget("history_maptype_label",
                                 xmLabelWidgetClass, H_FieldsContainer(xgh),
                                                  XmNtraversalOn, False,
                           XmNlabelString, XmStringCreateSimple("Map type"),
                                                  NULL);

    H_MaptypeText(xgh) = XtVaCreateManagedWidget("history_maptype_text",
                             xmTextFieldWidgetClass, H_FieldsContainer(xgh),
                                      XmNvalue, (H_MyHistory(xgh)->maptype),
                                                 XmNeditable, True,
                                                 XmNmaxLength, RECORD_LEN,
                                                 XmNcolumns, RECORD_LEN,
                                                 NULL);
    XtAddCallback(H_MaptypeText(xgh),XmNactivateCallback,HistoryNextField,xgh);
    XtAddCallback(H_MaptypeText(xgh), XmNlosingFocusCallback, HistoryUpdateStruct, xgh);

    H_Datsrc1Label(xgh) = XtVaCreateManagedWidget("history_datsrc_2_label",
                                 xmLabelWidgetClass, H_FieldsContainer(xgh),
                                                  XmNtraversalOn, False,
                        XmNlabelString, XmStringCreateSimple("Data source"),
                                                  NULL);

    H_Datsrc1Text(xgh) = XtVaCreateManagedWidget("history_datsrc_2_text",
                             xmTextFieldWidgetClass, H_FieldsContainer(xgh),
                                     XmNvalue, (H_MyHistory(xgh)->datsrc_1),
                                                 XmNeditable, True,
                                                 XmNmaxLength, RECORD_LEN,
                                                 XmNcolumns, RECORD_LEN,
                                                 NULL);
    XtAddCallback(H_Datsrc1Text(xgh),XmNactivateCallback,HistoryNextField,xgh);
    XtAddCallback(H_Datsrc1Text(xgh), XmNlosingFocusCallback, HistoryUpdateStruct, xgh);

    H_Datsrc2Label(xgh) = XtVaCreateManagedWidget("history_datsrc_2_label",
                                 xmLabelWidgetClass, H_FieldsContainer(xgh),
                                                  XmNtraversalOn, False,
                                  XmNlabelString, XmStringCreateSimple(" "),
                                                  NULL);

    H_Datsrc2Text(xgh) = XtVaCreateManagedWidget("history_datsrc_2_text",
                             xmTextFieldWidgetClass, H_FieldsContainer(xgh),
                                     XmNvalue, (H_MyHistory(xgh)->datsrc_2),
                                                 XmNeditable, True,
                                                 XmNmaxLength, RECORD_LEN,
                                                 XmNcolumns, RECORD_LEN,
                                                 NULL);
    XtAddCallback(H_Datsrc2Text(xgh),XmNactivateCallback,HistoryNextField,xgh);
    XtAddCallback(H_Datsrc2Text(xgh), XmNlosingFocusCallback, HistoryUpdateStruct, xgh);

    H_KeywrdLabel(xgh) = XtVaCreateManagedWidget("history_keywrd_label",
                                 xmLabelWidgetClass, H_FieldsContainer(xgh),
                                                 XmNtraversalOn, False,
                        XmNlabelString, XmStringCreateSimple("Description"),
                                                 NULL);

    H_KeywrdText(xgh) = XtVaCreateManagedWidget("history_keywrd_text",
                             xmTextFieldWidgetClass, H_FieldsContainer(xgh),
                                       XmNvalue, (H_MyHistory(xgh)->keywrd),
                                                XmNeditable, True,
                                                XmNmaxLength, RECORD_LEN,
                                                XmNcolumns, RECORD_LEN,
                                                NULL);
    XtAddCallback(H_KeywrdText(xgh),XmNactivateCallback,HistoryNextField,xgh);
    XtAddCallback(H_KeywrdText(xgh), XmNlosingFocusCallback, HistoryUpdateStruct, xgh);

    XtManageChild(form);
    return;
}

/****************************************************************/
static void
#ifdef _NO_PROTO
HistoryAdjustAndAttach(xgh, attach)
    HistoryWidget   xgh;
    Boolean         attach;
#else
HistoryAdjustAndAttach(HistoryWidget xgh, Boolean attach)
#endif
{
    Dimension       width = 0;
    Dimension       maxWidth = 0;
    Dimension       height = 0;
    Dimension       maxHeight = 0;

    if (attach) {
        XtVaSetValues(H_FieldsContainer(xgh),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(H_EdhistButton(xgh),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_FieldsContainer(xgh),
                      NULL);
        XtVaSetValues(H_EdhistLabel(xgh),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(H_EdhistText(xgh),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_EdhistLabel(xgh),
                      XmNbottomAttachment, XmATTACH_FORM,
                      NULL);

        XtVaSetValues(H_MapidLabel(xgh),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(H_TitleLabel(xgh),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_MapidLabel(xgh),
                      NULL);
        XtVaSetValues(H_MapsetLabel(xgh),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_TitleLabel(xgh),
                      NULL);
        XtVaSetValues(H_CreatorLabel(xgh),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_MapsetLabel(xgh),
                      NULL);
        XtVaSetValues(H_MaptypeLabel(xgh),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_CreatorLabel(xgh),
                      NULL);
        XtVaSetValues(H_Datsrc1Label(xgh),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_MaptypeLabel(xgh),
                      NULL);
        XtVaSetValues(H_Datsrc2Label(xgh),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_Datsrc1Label(xgh),
                      NULL);
        XtVaSetValues(H_KeywrdLabel(xgh),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_Datsrc2Label(xgh),
                      NULL);

        XtVaSetValues(H_MapidText(xgh),
        XmNleftAttachment, XmATTACH_WIDGET, XmNleftWidget, H_MapidLabel(xgh),
                      XmNtopAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(H_TitleText(xgh),
        XmNleftAttachment, XmATTACH_WIDGET, XmNleftWidget, H_TitleLabel(xgh),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_MapidText(xgh),
                      NULL);
        XtVaSetValues(H_MapsetText(xgh),
                      XmNleftAttachment, XmATTACH_WIDGET, XmNleftWidget, H_MapsetLabel(xgh),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_TitleText(xgh),
                      NULL);
        XtVaSetValues(H_CreatorText(xgh),
                      XmNleftAttachment, XmATTACH_WIDGET, XmNleftWidget, H_CreatorLabel(xgh),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_MapsetText(xgh),
                      NULL);
        XtVaSetValues(H_MaptypeText(xgh),
                      XmNleftAttachment, XmATTACH_WIDGET, XmNleftWidget, H_MaptypeLabel(xgh),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_CreatorText(xgh),
                      NULL);
        XtVaSetValues(H_Datsrc1Text(xgh),
                      XmNleftAttachment, XmATTACH_WIDGET, XmNleftWidget, H_Datsrc1Label(xgh),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_MaptypeText(xgh),
                      NULL);
        XtVaSetValues(H_Datsrc2Text(xgh),
                      XmNleftAttachment, XmATTACH_WIDGET, XmNleftWidget, H_Datsrc2Label(xgh),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_Datsrc1Text(xgh),
                      NULL);
        XtVaSetValues(H_KeywrdText(xgh),
                      XmNleftAttachment, XmATTACH_WIDGET, XmNleftWidget, H_KeywrdLabel(xgh),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, H_Datsrc2Text(xgh),
                      NULL);

    }
    maxWidth = 0;
    XtVaGetValues(H_MapidLabel(xgh), XmNwidth, &width, NULL);
    if (width > maxWidth)
        maxWidth = width;
    XtVaGetValues(H_TitleLabel(xgh), XmNwidth, &width, NULL);
    if (width > maxWidth)
        maxWidth = width;
    XtVaGetValues(H_MapsetLabel(xgh), XmNwidth, &width, NULL);
    if (width > maxWidth)
        maxWidth = width;
    XtVaGetValues(H_CreatorLabel(xgh), XmNwidth, &width, NULL);
    if (width > maxWidth)
        maxWidth = width;
    XtVaGetValues(H_MaptypeLabel(xgh), XmNwidth, &width, NULL);
    if (width > maxWidth)
        maxWidth = width;
    XtVaGetValues(H_Datsrc1Label(xgh), XmNwidth, &width, NULL);
    if (width > maxWidth)
        maxWidth = width;
    XtVaGetValues(H_Datsrc2Label(xgh), XmNwidth, &width, NULL);
    if (width > maxWidth)
        maxWidth = width;
    XtVaGetValues(H_KeywrdLabel(xgh), XmNwidth, &width, NULL);
    if (width > maxWidth)
        maxWidth = width;

    XtVaSetValues(H_MapidLabel(xgh), XmNwidth, maxWidth, NULL);
    XtVaSetValues(H_TitleLabel(xgh), XmNwidth, maxWidth, NULL);
    XtVaSetValues(H_MapsetLabel(xgh), XmNwidth, maxWidth, NULL);
    XtVaSetValues(H_CreatorLabel(xgh), XmNwidth, maxWidth, NULL);
    XtVaSetValues(H_MaptypeLabel(xgh), XmNwidth, maxWidth, NULL);
    XtVaSetValues(H_Datsrc1Label(xgh), XmNwidth, maxWidth, NULL);
    XtVaSetValues(H_Datsrc2Label(xgh), XmNwidth, maxWidth, NULL);
    XtVaSetValues(H_KeywrdLabel(xgh), XmNwidth, maxWidth, NULL);

    XtVaGetValues(H_MapidText(xgh), XmNheight, &height, NULL);

    XtVaSetValues(H_MapidLabel(xgh), XmNheight, height, NULL);
    XtVaSetValues(H_TitleLabel(xgh), XmNheight, height, NULL);
    XtVaSetValues(H_MapsetLabel(xgh), XmNheight, height, NULL);
    XtVaSetValues(H_CreatorLabel(xgh), XmNheight, height, NULL);
    XtVaSetValues(H_MaptypeLabel(xgh), XmNheight, height, NULL);
    XtVaSetValues(H_Datsrc1Label(xgh), XmNheight, height, NULL);
    XtVaSetValues(H_Datsrc2Label(xgh), XmNheight, height, NULL);
    XtVaSetValues(H_KeywrdLabel(xgh), XmNheight, height, NULL);
}

/****************************************************************/
static void
#ifdef _NO_PROTO
Destroy(xgh)
    HistoryWidget   xgh;
#else
Destroy(
        HistoryWidget xgh)
#endif
/****************
 *
 ****************/
{
    /****************/

    return;
}

/****************************************************************/
static void
#ifdef _NO_PROTO
DeleteChild(w)
    Widget          w;
#else
DeleteChild(
            Widget w)
#endif
/****************
 * This procedure is called to remove the child from
 *   the child list, and to allow the parent to do any
 *   neccessary clean up.
 ****************/
{
    HistoryWidget   xgh;
    Widget          child = w;
    /****************/

    xgh = (HistoryWidget) w;
    while (!XgIsHistory(xgh))
        xgh = (HistoryWidget) XtParent(xgh);

    (*((InteractorWidgetClass) interactorWidgetClass)
     ->composite_class.delete_child) (w);
}

/****************************************************************/
static          Boolean
#ifdef _NO_PROTO
SetValues(current, request, new)
    HistoryWidget   current;
    HistoryWidget   request;
    HistoryWidget   new;
#else
SetValues(
          HistoryWidget current,
          HistoryWidget request,
          HistoryWidget new)
#endif
/****************
 * This routine detects differences in two versions
 *   of a widget, when a difference is found the
 *   appropriate action is taken.
 ****************/
{
    Arg             args[10];
    int             n;
    String          newString;
    /****************/

    BB_InSetValues(new) = TRUE;

    /*
     * Current, no History resources are modifiable through XtSetValues
     */

    BB_InSetValues(new) = FALSE;

    if (XtClass(new) == historyWidgetClass) {
        _XmBulletinBoardSizeUpdate((XmBulletinBoardWidget) new);
    }
    return (FALSE);
}

/****************************************************************/
static void
#ifdef _NO_PROTO
XgHistoryCallBack(wid, which_button, callback)
    Widget          wid;
    XtPointer       which_button;
    XmAnyCallbackStruct *callback;
#else
XgHistoryCallBack(Widget wid,
                  XtPointer which_button,
                  XmAnyCallbackStruct * callback)
#endif
/****************
 * This is the procedure which does all of the button
 *   callback magic.
 ****************/
{
    InteractorCallbackStruct callbackStruct;
    HistoryWidget   xgh;
    Boolean         allowUnmanage = FALSE;
    /****************/

    xgh = (HistoryWidget) XtParent(wid);
    callbackStruct.value = (XmString) which_button;
    callbackStruct.event = callback->event;

    switch ((int) which_button) {
    case XmINTERACT_OK_BUTTON:
    case XmINTERACT_APPLY_BUTTON:
        break;
    case XmINTERACT_CANCEL_BUTTON:
        break;
    case XmINTERACT_HELP_BUTTON:
        break;
    }

}

/****************************************************************/
Widget
#ifdef _NO_PROTO
XgCreateHistory(p, name, args, n)
    Widget          p;          /* parent widget   */
    String          name;       /* widget name     */
    ArgList         args;       /* arg list        */
    Cardinal        n;          /* arg count       */
#else
XgCreateHistory(
                Widget p,       /* parent widget   */
                String name,    /* widget name     */
                ArgList args,   /* arg list        */
                Cardinal n)     /* arg count       */
#endif
/****************
 *
 ****************/
{
    /****************/

    return (XtCreateWidget(name, historyWidgetClass, p, args, n));
}

/****************************************************************/
Widget
#ifdef _NO_PROTO
XgCreateHistoryDialog(ds_p, name, xgh_args, xgh_n)
    Widget          ds_p;       /* parent for shell    */
    String          name;       /* widget name         */
    ArgList         xgh_args;   /* arglist for xgh      */
    Cardinal        xgh_n;      /* argcount for xgh     */
#else
XgCreateHistoryDialog(
                      Widget ds_p,      /* parent for shell    */
                      String name,      /* widget name         */
                      ArgList xgh_args, /* arglist for xgh      */
                      Cardinal xgh_n)   /* argcount for xgh     */
#endif
/****************
 * This convenience function creates a DialogShell
 *   and a History child of the shell;
 *   returns the History widget.
 ****************/
{
    Widget          xgh;        /* new xgh widget      */
    Widget          ds;         /* DialogShell         */
    Arg             ds_args[10];/* arglist for shell  */
    char           *ds_name;
    /****************/

    /*
     * Create DialogShell parent.
     */
    ds_name = XtCalloc(strlen(name) + 1, sizeof(char));
    strcpy(ds_name, name);

    XtSetArg(ds_args[0], XmNallowShellResize, True);
    ds = XmCreateDialogShell(ds_p, ds_name, ds_args, 1);

    XtFree(ds_name);

    /*
     * Create FileSelectionBox.
     */
    xgh = XtCreateWidget(name, historyWidgetClass, ds,
                         xgh_args, xgh_n);
    XtAddCallback(xgh, XmNdestroyCallback, _XmDestroyParentCallback, NULL);

    return (xgh);
}
