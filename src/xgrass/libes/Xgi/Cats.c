static char rcsid[] = "@(#)XGRASS $Id: Cats.c,v 0.0 1992/05/05 14:55:51 sink Exp sink $";
/*
 * File: Cats.
 *
 * Desc: Implementation of Cats widget
 *
 * Auth: Eric W. Sink
 *
 * Date: 24 Feb 1992
 *
 * Modification History:
 *
 *
 */

#include <CatsP.h>

#include <X11/StringDefs.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/TextF.h>
#include <Xm/Text.h>
#include <Xm/ArrowBG.h>
#include <Xm/List.h>
#include <Xm/DrawingA.h>
#include <Xm/Scale.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/DialogS.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include "XmLinux.h"
#include "gis.h"
#include "string.h"

#include <stdio.h>
#include <ctype.h>

extern char    *_XgStrDup();
extern void    *_XgMalloc();

/*---------------------------------------------------*/
/* forward declarations                              */
/* */
/* this is a list of all private procedures in this  */
/* module                                            */
/*---------------------------------------------------*/

#ifdef _NO_PROTO

static void     CatsAddCat();
static void     IncrementTextVerifyCallBack();
static void     IncrementTextCallBack();
static void     IncrementCallBack();
static void     ClassInitialize();
static void     ClassPartInitialize();
static void     Initialize();
static void     Destroy();
static void     DeleteChild();
static Boolean  SetValues();
static void     XgCatsCallBack();
static void     CatsDataSetup();
static void     CatsCreateWidgets();
static void     CatsAdjustAndAttach();

#else                           /* _NO_PROTO */

static void CatsAddCat(Widget,XtPointer,XmListCallbackStruct *);

static void
IncrementTextVerifyCallBack(
                            Widget wid,
                            XtPointer client_data,
                            XtPointer call_data);
static void
IncrementTextCallBack(
                      Widget wid,
                      XtPointer client_data,
                      XtPointer call_data);
static void
IncrementCallBack(
                  Widget wid,
                  XtPointer client_data,
                  XtPointer call_data);

static void     ClassInitialize();
static void     ClassPartInitialize(CatsWidgetClass xgcc);
static void
                Initialize(CatsWidget request, CatsWidget new);
static void     Destroy(CatsWidget xgc);
static void     DeleteChild(Widget w);
static void     CatsDataSetup(Widget);
static void     CatsBuildResult(Widget);
static Boolean
SetValues(CatsWidget current,
          CatsWidget request,
          CatsWidget new);
static void
XgCatsCallBack(Widget wid,
               XtPointer which_button,
               XmAnyCallbackStruct * callback);
static void     CatsCreateWidgets(CatsWidget xgc);
static void     CatsAdjustAndAttach(CatsWidget xgc, Boolean attach);

#endif                          /* _NO_PROTO */

/*---------------------------------------------------*/
/* widget resources                                  */
/*---------------------------------------------------*/
static XtResource resources[] =
{
    /* Cats specific resources */

    {XmNrange,
        XmCRange,
        XmRRangeStructPtr,
        sizeof(struct Range *),
        XtOffset(CatsWidget, cats.myRange),
        XmRImmediate,
        (XtPointer) 0
    },
    {XmNcats,
        XmCCats,
        XmRCatsStructPtr,
        sizeof(struct Categories *),
        XtOffset(CatsWidget, cats.myCats),
        XmRImmediate,
        (XtPointer) 0
    },

    /* superclass resource default overrides */

    {XmNautoUnmanage,
        XmCAutoUnmanage,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(CatsWidget, bulletin_board.auto_unmanage),
        XmRImmediate,
        (XtPointer) TRUE
    },
    {XmNdialogType,
        XmCDialogType,
        XmRDialogType,
        sizeof(unsigned char),
        XtOffset(CatsWidget, interactor.dialog_type),
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

externaldef(xgcatsclassrec)
    CatsClassRec    catsClassRec =
    {
        {                       /* core class record        */
             /* superclass          */ (WidgetClass) & interactorClassRec,
             /* class_name          */ "Cats",
             /* widget_size         */ sizeof(CatsRec),
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
        {                       /* cats class record */
             /* extension */ NULL,
        }
    };

externaldef(xgcatswidgetclass) WidgetClass
catsWidgetClass = (WidgetClass) & catsClassRec;

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
ClassPartInitialize(xgc)
    CatsWidgetClass xgc;
#else
ClassPartInitialize(CatsWidgetClass xgc)
#endif
/****************
 * Class Initialization.  Sets up accelerators and fast subclassing.
 ****************/
{
    /****************/

    _XmFastSubclassInit(xgc, XmCATS_BIT);

    return;
}

static void
CatsNameEdit(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    Widget          xgc = (Widget) data;
    char           *newItem;
    char           *label;
    CELL            n;
    /* Change the list entry */
    if (C_CurrentCat(xgc)) {
        char            buf[2048];      /* ...because Michael said so... */


        newItem = XmTextFieldGetString(C_NameText(xgc));
        if (*newItem == NULL)
            sprintf(buf, "%d: no-label", C_CategoryValueIncrementValue(xgc));
        else
            sprintf(buf, "%d: %s", C_CategoryValueIncrementValue(xgc), newItem);
        C_CatItems(xgc)[C_CurrentCat(xgc) - 1] = XmStringCreateSimple(buf);
        XtVaSetValues(C_CategoriesList(xgc),
                      XmNitems, C_CatItems(xgc),
                      NULL);
        G_set_cat(C_CategoryValueIncrementValue(xgc), newItem, C_MyCats(xgc));
        G_update_range(C_CategoryValueIncrementValue(xgc), C_MyRange(xgc));
    } else {
        CatsAddCat(xgc, (XtPointer) xgc, NULL);
    }
}

/****************************************************************/
static void
#ifdef _NO_PROTO
Initialize(request, new)
    CatsWidget      request;
    CatsWidget      new;
#else
Initialize(CatsWidget request,
           CatsWidget new)
#endif
/****************
 * This routine initializes an instance of the cats widget.
 * Instance record fields which are shadow resources for child widgets and
 *   which are of an allocated type are set to NULL after they are used, since
 *   the memory identified by them is not owned by the cats widget.
 ****************/
{
    Arg             args[16];
    int             numArgs;
    /****************/

    /*
     * Create all of the new widgets and initialize internal data strux.
     */
    CatsCreateWidgets(new);

    if (request->manager.navigation_type != XmEXCLUSIVE_TAB_GROUP)
        _XmChangeNavigationType((Widget) new, ((XtIsShell(XtParent(request))
                                       ? XmSTICKY_TAB_GROUP : XmTAB_GROUP)));

    CatsAdjustAndAttach(new, True);

    return;
}

static void
CatsUpdateTitle(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XtPointer       cbs;
{
    Widget          xgc = (Widget) data;
    G_set_cats_title(XmTextFieldGetString(C_TitleText(xgc)), C_MyCats(xgc));
}

void
XgPutCatsInList(cats, range, list, xgc)
    struct Categories *cats;
    struct Range   *range;
    Widget          list;
    Widget          xgc;
{
    int             numItems = 0;
    CELL            i;
    CELL            min, max;

    G_get_range_min_max(range, &min, &max);

    numItems = max - min + 1;
    C_CatItems(xgc) = (XmStringTable) XtCalloc(
                                               numItems, sizeof(XmString));

    for (i = min; i <= max; i++) {
        char            buf[2048];      /* ...because Michael said so... */
        char           *label;

        label = G_get_cat((CELL) i, cats);

        if (*label == NULL)
            sprintf(buf, "%d: no-label", i);
        else
            sprintf(buf, "%d: %s", i, label);
        C_CatItems(xgc)[i - min] = XmStringCreateSimple(buf);
    }

    C_CatItemsCount(xgc) = numItems;

    XtVaSetValues(list,
                  XmNitems, C_CatItems(xgc),
                  XmNitemCount, numItems,
                  NULL);
}

static void
CatsAddCat(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XmListCallbackStruct *cbs;
{
    Widget          xgc = (Widget) data;
    char           *newItem;
    char            buf[2048];  /* ...because Michael said so... */
    char           *oldItem;

    oldItem = G_get_cat((CELL) C_CategoryValueIncrementValue(xgc), C_MyCats(xgc));
    if (*oldItem) {
        XBell(XtDisplay(xgc), 73);
        return;
    }
    newItem = XmTextFieldGetString(C_NameText(xgc));
    if (*newItem == NULL)
        sprintf(buf, "%d: no-label", C_CategoryValueIncrementValue(xgc));
    else
        sprintf(buf, "%d: %s", C_CategoryValueIncrementValue(xgc), newItem);
    C_CatItems(xgc) = (void *) XtRealloc(C_CatItems(xgc), ((++C_CatItemsCount(xgc)) * sizeof(XmString)));
    C_CatItems(xgc)[C_CatItemsCount(xgc) - 1] = XmStringCreateSimple(buf);
    XtVaSetValues(C_CategoriesList(xgc),
                  XmNitems, C_CatItems(xgc),
                  XmNitemCount, C_CatItemsCount(xgc),
                  NULL);
    G_set_cat(C_CategoryValueIncrementValue(xgc), newItem, C_MyCats(xgc));
    G_update_range(C_CategoryValueIncrementValue(xgc), C_MyRange(xgc));
}

static void
CatsListCallback(w, data, cbs)
    Widget          w;
    XtPointer       data;
    XmListCallbackStruct *cbs;
{
    Widget          xgc = (Widget) data;
    char           *choice;
    int             pos;
    char            new_text[10];
    pos = cbs->item_position;
    if (pos == C_CurrentCat(xgc)) {
        C_CurrentCat(xgc) = 0;
        XmTextFieldSetString(C_NameText(xgc), "");
    } else {
        char           *label;
        XtSetSensitive(C_AddButton(xgc), False);
        C_CurrentCat(xgc) = pos;
        XmStringGetLtoR(cbs->item, XmSTRING_DEFAULT_CHARSET, &choice);
        C_CategoryValueIncrementValue(xgc) = atoi(choice);
        sprintf(new_text, "%d", C_CategoryValueIncrementValue(xgc));
        XmTextFieldSetString(C_CategoryValueIncrementText(xgc), new_text);
        label = choice;
        while (*label != ' ') {
            label++;
        }
        label++;
        XmTextFieldSetString(C_NameText(xgc), label);
    }
}

/****************************************************************/
static void
#ifdef _NO_PROTO
CatsCreateWidgets(xgc)
    CatsWidget      xgc;
#else
CatsCreateWidgets(CatsWidget xgc)
#endif
{
    Widget          form;
    Arg             al[10];
    int             ac = 0;
    Arg             args[5];
    int             nargs = 0;
    XmString        s;
    Widget          firstItem;
    int             i;

    form = XmCreateForm((Widget) xgc, "xgcats_form", args, nargs);
    XgAddHelpCallBackFromFile(I_WorkArea(xgc),"xgcats_form");

    C_TitleLabel(xgc) = XtVaCreateManagedWidget("cats_title_label", xmLabelWidgetClass, form,
                          XmNlabelString, XmStringCreateSimple("Map title"),
                                                XmNtraversalOn, False,
                                                NULL);

    C_TitleText(xgc) = XtVaCreateManagedWidget("cats_text", xmTextFieldWidgetClass, form,
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                               NULL);
    XgAddHelpCallBackFromFile(C_TitleText(xgc), "cats_text");
    XmTextFieldSetString(C_TitleText(xgc), G_get_cats_title(C_MyCats(xgc)));
    XtAddCallback(C_TitleText(xgc), XmNlosingFocusCallback, CatsUpdateTitle, xgc);
    XtAddCallback(C_TitleText(xgc), XmNactivateCallback, CatsUpdateTitle, xgc);

    C_TitleSeparator(xgc) = XtVaCreateManagedWidget("cats_title_separator", xmSeparatorWidgetClass, form,
                                               XmNorientation, XmHORIZONTAL,
                                                    NULL);

    C_LowerContainer(xgc) = XtVaCreateManagedWidget("cats_lower_container", xmRowColumnWidgetClass, form,
                                               XmNorientation, XmHORIZONTAL,
                                                    XmNpacking, XmPACK_TIGHT,
                                                    NULL);

    C_ListContainer(xgc) = XtVaCreateManagedWidget("cats_buttons_container", xmRowColumnWidgetClass, C_LowerContainer(xgc),
                                                 XmNorientation, XmVERTICAL,
                                                   XmNpacking, XmPACK_TIGHT,
                                                   NULL);

    C_CategoriesLabel(xgc) = XtVaCreateManagedWidget("cats_categories_label", xmLabelWidgetClass, C_ListContainer(xgc),
                         XmNlabelString, XmStringCreateSimple("Categories"),
                                                     XmNtraversalOn, False,
                                                     NULL);

    nargs = 0;
    XtSetArg(args[nargs], XmNvisibleItemCount, 8);
    nargs++;
    XtSetArg(args[nargs], XmNselectionPolicy, XmSINGLE_SELECT);
    nargs++;
    C_CategoriesList(xgc) = XmCreateScrolledList(C_ListContainer(xgc), "cats_cat_list", args, nargs);
    XgPutCatsInList(C_MyCats(xgc), C_MyRange(xgc), C_CategoriesList(xgc), xgc);
    XtManageChild(C_CategoriesList(xgc));
    XtAddCallback(C_CategoriesList(xgc), XmNsingleSelectionCallback, CatsListCallback, xgc);

    XgAddHelpCallBackFromFile(C_CategoriesList(xgc), "cats_categories_list");

    C_ButtonsContainer(xgc) = XtVaCreateManagedWidget("cats_buttons_container", xmRowColumnWidgetClass, C_LowerContainer(xgc),
                                                 XmNorientation, XmVERTICAL,
                                                   XmNpacking, XmPACK_TIGHT,
                                                      NULL);

    {
        char            help[256];
        int             numItems = 0;
        Dimension       width = 0;
        Dimension       height = 0;

        ac = 0;
        C_CategoryValueIncrementFrame(xgc) = XmCreateFrame(C_ButtonsContainer(xgc),
                                  "category_value_increment_frame", al, ac);
        XtManageChild(C_CategoryValueIncrementFrame(xgc));

        ac = 0;
        C_CategoryValueIncrementForm(xgc) = XmCreateForm(
                                         C_CategoryValueIncrementFrame(xgc),
                                   "category_value_increment_form", al, ac);
        XtManageChild(C_CategoryValueIncrementForm(xgc));

        ac = 0;
        if (C_CategoryValueLabelString(xgc) != NULL) {
            XtSetArg(al[ac], XmNlabelString, C_CategoryValueLabelString(xgc));
            ac++;
        }
        XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
        ac++;
        XtSetArg(al[ac], XmNtraversalOn, False);
        ++ac;
        C_CategoryValueLabel(xgc) = XmCreateLabelGadget(
              C_CategoryValueIncrementForm(xgc), "Category value:", al, ac);
        XtManageChild(C_CategoryValueLabel(xgc));

        ac = 0;
        XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
        ++ac;
        XtSetArg(al[ac], XmNarrowDirection, XmARROW_UP);
        ++ac;
        C_CategoryValueIncrementUp(xgc) = XmCreateArrowButtonGadget(
            C_CategoryValueIncrementForm(xgc), "cats_inc_up", al, ac);
        XgAddHelpCallBackFromFile(C_CategoryValueIncrementUp(xgc), "cats_inc_up");
        XtManageChild(C_CategoryValueIncrementUp(xgc));
        XtAddCallback(C_CategoryValueIncrementUp(xgc), XmNactivateCallback,
                      IncrementCallBack, (XtPointer) xgc);

        ac = 0;
        XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
        ++ac;
        XtSetArg(al[ac], XmNarrowDirection, XmARROW_DOWN);
        ++ac;
        C_CategoryValueIncrementDown(xgc) = XmCreateArrowButtonGadget(
          C_CategoryValueIncrementForm(xgc), "cats_inc_down", al, ac);
        XgAddHelpCallBackFromFile(C_CategoryValueIncrementDown(xgc), "cats_inc_down");
        XtManageChild(C_CategoryValueIncrementDown(xgc));
        XtAddCallback(C_CategoryValueIncrementDown(xgc), XmNactivateCallback,
                      IncrementCallBack, (XtPointer) xgc);

        ac = 0;
        XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
        ++ac;
        C_CategoryValueIncrementText(xgc) = XmCreateTextField(
                                          C_CategoryValueIncrementForm(xgc),
                                             "cats_inc_text", al, ac);
        XgAddHelpCallBackFromFile(C_CategoryValueIncrementText(xgc), "cats_inc_text");
        XtManageChild(C_CategoryValueIncrementText(xgc));
        C_CategoryValueIncrementValue(xgc) = 0;
        XmTextFieldInsert(C_CategoryValueIncrementText(xgc), 0, "0");
        XtAddCallback(C_CategoryValueIncrementText(xgc), XmNactivateCallback,
                      IncrementTextCallBack, (XtPointer) xgc);
        XtAddCallback(C_CategoryValueIncrementText(xgc), XmNmodifyVerifyCallback,
                      IncrementTextVerifyCallBack, (XtPointer) xgc);

        XtVaSetValues(C_CategoryValueLabel(xgc),
                      XmNtopAttachment, XmATTACH_FORM,
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(C_CategoryValueIncrementUp(xgc),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, C_CategoryValueLabel(xgc),
                      XmNleftAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(C_CategoryValueIncrementDown(xgc),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, C_CategoryValueIncrementUp(xgc),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNbottomAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(C_CategoryValueIncrementText(xgc),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, C_CategoryValueLabel(xgc),
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNleftAttachment, XmATTACH_WIDGET,
                      XmNleftWidget, C_CategoryValueIncrementUp(xgc),
                      XmNbottomAttachment, XmATTACH_FORM,
                      NULL);

    }
    ac = 0;
    C_NameFrame(xgc) = XmCreateFrame(C_ButtonsContainer(xgc),
                                     "cats_name_frame", al, ac);
    XtManageChild(C_NameFrame(xgc));

    ac = 0;
    C_NameForm(xgc) = XmCreateForm(
                                   C_NameFrame(xgc),
                                   "cats_name_form", al, ac);
    XtManageChild(C_NameForm(xgc));

    ac = 0;
    XtSetArg(al[ac], XmNlabelString, XmStringCreateSimple("Category name:"));
    ac++;
    XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
    ac++;
    XtSetArg(al[ac], XmNtraversalOn, False);
    ++ac;
    C_NameLabel(xgc) = XmCreateLabelGadget(
                                 C_NameForm(xgc), "Category name:", al, ac);
    XtManageChild(C_NameLabel(xgc));

    C_NameText(xgc) = XtVaCreateManagedWidget("cats_name_text", xmTextFieldWidgetClass, C_NameForm(xgc),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                                              NULL);
    XtVaSetValues(C_NameLabel(xgc),
                  XmNleftAttachment, XmATTACH_FORM,
                  XmNrightAttachment, XmATTACH_FORM,
                  XmNtopAttachment, XmATTACH_FORM,
                  NULL);

    XtVaSetValues(C_NameText(xgc),
                  XmNleftAttachment, XmATTACH_FORM,
                  XmNrightAttachment, XmATTACH_FORM,
                  XmNtopAttachment, XmATTACH_WIDGET,
                  XmNtopWidget, C_NameLabel(xgc),
                  NULL);

    XgAddHelpCallBackFromFile(C_NameText(xgc), "cats_name_text");
    XtAddCallback(C_NameText(xgc), XmNactivateCallback, CatsNameEdit, xgc);

    C_AddButton(xgc) = XtVaCreateManagedWidget("catsaddbutton", xmPushButtonWidgetClass, C_ButtonsContainer(xgc),
                                      XmNnavigationType, XmSTICKY_TAB_GROUP,
                       XmNlabelString, XmStringCreateSimple("Add category"),
                                               NULL);
    XgAddHelpCallBackFromFile(C_AddButton(xgc), "cats_add_button");
    XtAddCallback(C_AddButton(xgc), XmNactivateCallback, CatsAddCat, xgc);

    XtManageChild(form);
    return;
}

/****************************************************************/
static void
#ifdef _NO_PROTO
CatsAdjustAndAttach(xgc, attach)
    CatsWidget      xgc;
    Boolean         attach;
#else
CatsAdjustAndAttach(CatsWidget xgc, Boolean attach)
#endif
{
    Dimension       width = 0;
    Dimension       maxWidth = 0;
    Dimension       height = 0;
    Dimension       maxHeight = 0;

    if (attach) {
        XtVaSetValues(C_TitleLabel(xgc),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(C_TitleText(xgc),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, C_TitleLabel(xgc),
                      NULL);
        XtVaSetValues(C_TitleSeparator(xgc),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, C_TitleText(xgc),
                      NULL);
        XtVaSetValues(C_LowerContainer(xgc),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNbottomAttachment, XmATTACH_FORM,
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, C_TitleSeparator(xgc),
                      NULL);
    }
}

/****************************************************************/
static void
#ifdef _NO_PROTO
Destroy(xgc)
    CatsWidget      xgc;
#else
Destroy(
        CatsWidget xgc)
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
    CatsWidget      xgc;
    Widget          child = w;
    /****************/

    xgc = (CatsWidget) w;
    while (!XgIsCats(xgc))
        xgc = (CatsWidget) XtParent(xgc);

    (*((InteractorWidgetClass) interactorWidgetClass)
     ->composite_class.delete_child) (w);
  }

/****************************************************************/
static          Boolean
#ifdef _NO_PROTO
SetValues(current, request, new)
    CatsWidget      current;
    CatsWidget      request;
    CatsWidget      new;
#else
SetValues(
          CatsWidget current,
          CatsWidget request,
          CatsWidget new)
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
     * Current, no Cats resources are modifiable through XtSetValues
     */

    BB_InSetValues(new) = FALSE;

    if (XtClass(new) == catsWidgetClass) {
        _XmBulletinBoardSizeUpdate((XmBulletinBoardWidget) new);
    }
    return (FALSE);
}

/****************************************************************/
static void
#ifdef _NO_PROTO
XgCatsCallBack(wid, which_button, callback)
    Widget          wid;
    XtPointer       which_button;
    XmAnyCallbackStruct *callback;
#else
XgCatsCallBack(Widget wid,
               XtPointer which_button,
               XmAnyCallbackStruct * callback)
#endif
/****************
 * This is the procedure which does all of the button
 *   callback magic.
 ****************/
{
    InteractorCallbackStruct callbackStruct;
    CatsWidget      xgc;
    Boolean         allowUnmanage = FALSE;
    /****************/

    xgc = (CatsWidget) XtParent(wid);
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
XgCreateCats(p, name, args, n)
    Widget          p;          /* parent widget   */
    String          name;       /* widget name     */
    ArgList         args;       /* arg list        */
    Cardinal        n;          /* arg count       */
#else
XgCreateCats(
             Widget p,          /* parent widget   */
             String name,       /* widget name     */
             ArgList args,      /* arg list        */
             Cardinal n)        /* arg count       */
#endif
/****************
 *
 ****************/
{
    /****************/

    return (XtCreateWidget(name, catsWidgetClass, p, args, n));
}

/****************************************************************/
Widget
#ifdef _NO_PROTO
XgCreateCatsDialog(ds_p, name, xgc_args, xgc_n)
    Widget          ds_p;       /* parent for shell    */
    String          name;       /* widget name         */
    ArgList         xgc_args;   /* arglist for xgc      */
    Cardinal        xgc_n;      /* argcount for xgc     */
#else
XgCreateCatsDialog(
                   Widget ds_p, /* parent for shell    */
                   String name, /* widget name         */
                   ArgList xgc_args,    /* arglist for xgc      */
                   Cardinal xgc_n)      /* argcount for xgc     */
#endif
/****************
 * This convenience function creates a DialogShell
 *   and a Cats child of the shell;
 *   returns the Cats widget.
 ****************/
{
    Widget          xgc;        /* new xgc widget      */
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
    xgc = XtCreateWidget(name, catsWidgetClass, ds,
                         xgc_args, xgc_n);
    XtAddCallback(xgc, XmNdestroyCallback, _XmDestroyParentCallback, NULL);

    return (xgc);
}


static void
#ifdef _NO_PROTO
IncrementTextVerifyCallBack(w, client_data, call_data)
    Widget          w;
    XtPointer       client_data;
    XtPointer       call_data;
#else
IncrementTextVerifyCallBack(
                       Widget w, XtPointer client_data, XtPointer call_data)
#endif
{
    CatsWidget      xgc = (CatsWidget) client_data;
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *) call_data;
    int             i;

    for (i = 0; i < cbs->text->length; i++) {
        /* disallow non-digits, but allow a '-' in position 0 */
        if ((cbs->startPos + i) == 0) {
            if (((!isdigit(*(cbs->text->ptr + i)))) && (*(cbs->text->ptr + i)) != '-') {
                cbs->doit = False;
                XBell(XtDisplay(xgc), 0);
                return;
            }
        } else {
            if (!isdigit(*(cbs->text->ptr + i))) {
                cbs->doit = False;
                XBell(XtDisplay(xgc), 0);
                return;
            }
        }
    }
    cbs->doit = True;
}

static void
#ifdef _NO_PROTO
IncrementTextCallBack(w, client_data, call_data)
    Widget          w;
    XtPointer       client_data;
    XtPointer       call_data;
#else
IncrementTextCallBack(Widget w, XtPointer client_data, XtPointer call_data)
#endif
{
    CatsWidget      xgc = (CatsWidget) client_data;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
    char           *text;
    int             value;

    text = XmTextFieldGetString(C_CategoryValueIncrementText(xgc));
    value = atoi(text);
    C_CategoryValueIncrementValue(xgc) = value;

    C_CurrentCat(xgc) = 0;
    XmListDeselectAllItems(C_CategoriesList(xgc));
    XtSetSensitive(C_AddButton(xgc), True);
}

static void
#ifdef _NO_PROTO
IncrementCallBack(w, client_data, call_data)
    Widget          w;
    XtPointer       client_data;
    XtPointer       call_data;
#else
IncrementCallBack(Widget w, XtPointer client_data, XtPointer call_data)
#endif
{
    CatsWidget      xgc = (CatsWidget) client_data;
    char           *text;
    int             value;
    char            new_text[10];


    text = XmTextFieldGetString(C_CategoryValueIncrementText(xgc));
    value = atoi(text);
    if (w == C_CategoryValueIncrementUp(xgc)) {
        value++;
    } else {
        value--;
    }
    C_CategoryValueIncrementValue(xgc) = value;
    sprintf(new_text, "%d", value);
    XmTextFieldSetString(C_CategoryValueIncrementText(xgc), new_text);
    C_CurrentCat(xgc) = 0;
    XmListDeselectAllItems(C_CategoriesList(xgc));
    XtSetSensitive(C_AddButton(xgc), True);
}
