#include <ReclassP.h>
#include <Browser.h>
#include <Interact.h>
#include <help.h>
#include <xgbitmaps.h>
#include <xgrass_lib.h>

#include <X11/StringDefs.h>
#include <Xm/ArrowBG.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/LabelG.h>
#include <Xm/MessageB.h>
#include <Xm/List.h>
#include <Xm/TextF.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/DialogS.h>
#include "XmLinux.h"

#include <stdio.h>
#include <ctype.h>

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
static void     XgReclassCallBack();
static void     BrowserCallBack();
static void     BrowserOkCallBack();
static void     NewMapTextCallBack();
static void     OriginalMapTextCallBack();
static void     NewMapTitleTextCallBack();
static void     IncrementTextVerifyCallBack();
static void     IncrementTextCallBack();
static void     IncrementCallBack();
static void     GroupCallBack();
static void     UngroupCallBack();
static void     ReclassCreateWidgets();
static int      CreateOriginalMapListItems();
static void     FreeOriginalMapListItems();
static void     ReclassAdjustAndAttach();
static void     RemoveHelpCallbacks();
static void     AddHelpCallbacks();
static XgReclassRule AllocReclassRuleStruct();
static XgReclassRule FindReclassRule();
static void     LinkReclassRule();
static void     FreeReclassRuleList();
static void     ExecuteReclass();
static void     FinishExecute();
static char    *StripNewLine();

#else				/* _NO_PROTO */

static void     ClassInitialize();
static void     ClassPartInitialize(ReclassWidgetClass xgrc);
static void
Initialize(
	   ReclassWidget request,
	   ReclassWidget new);
static void     Destroy(ReclassWidget xgr);
static void     DeleteChild(Widget w);
static          Boolean
SetValues(
	  ReclassWidget current,
	  ReclassWidget request,
	  ReclassWidget new);
static void
XgReclassCallBack(
		  Widget wid,
		  XtPointer which_button,
		  XtPointer callback);
static void
BrowserCallBack(
		Widget wid,
		XtPointer client_data,
		XtPointer call_data);
static void
BrowserOkCallBack(
		  Widget wid,
		  XtPointer client_data,
		  XtPointer call_data);
static void
NewMapTextCallBack(
		   Widget wid,
		   XtPointer client_data,
		   XtPointer call_data);
static void
OriginalMapTextCallBack(
			Widget wid,
			XtPointer client_data,
			XtPointer call_data);
static void
NewMapTitleTextCallBack(
			Widget wid,
			XtPointer client_data,
			XtPointer call_data);
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
static void
GroupCallBack(
	      Widget wid,
	      XtPointer client_data,
	      XtPointer call_data);
static void
UngroupCallBack(
		Widget wid,
		XtPointer client_data,
		XtPointer call_data);
static void
                ReclassCreateWidgets(ReclassWidget xgr);
static int
                CreateOriginalMapListItems(ReclassWidget xgr);
static void
                FreeOriginalMapListItems(ReclassWidget xgr);
static void
                ReclassAdjustAndAttach(ReclassWidget xgr, Boolean attach);
static void
RemoveHelpCallbacks(
		    ReclassWidget xgr);
static void
AddHelpCallbacks(
		 int which,
		 ReclassWidget xgr);
static XgReclassRule AllocReclassRuleStruct();
static          XgReclassRule
FindReclassRule(
		ReclassWidget xgr,
		XgReclassRule rule);
static void
LinkReclassRule(
		ReclassWidget xgr,
		XgReclassRule rule);
static void     FreeReclassRuleList(XgReclassRule rule);
static void     ExecuteReclass(ReclassWidget xgr);
static void
FinishExecute(
	      Widget wid,
	      XtPointer client_data,
	      XtPointer call_data);
static char    *
                StripNewLine(char *s);

#endif				/* _NO_PROTO */

#ifndef MCCABE
static char     defaultTranslations[] =
"<Key>osfSelect:         ManagerGadgetSelect()\n\
        <Key>osfHelp:           XgInteractorHelp()\n\
        <Key>osfCancel:         BulletinBoardCancel()\n\
        ~Shift ~Meta ~Alt <Key>space:   ManagerGadgetSelect()\n\
        <Key>:                  ManagerGadgetKeyInput()\n\
        <BtnMotion>:    ManagerGadgetButtonMotion()\n\
        <Btn1Down>:     ManagerGadgetArm()\n\
        <Btn1Down>,<Btn1Up>:    ManagerGadgetActivate()\n\
        <Btn1Up>:       ManagerGadgetActivate()\n\
        <Btn1Down>(2+): ManagerGadgetMultiArm()\n\
        <Btn1Up>(2+):   ManagerGadgetMultiActivate()";

static char     defaultAccelerators[] =
"\043override\n\
        <Key>osfCancel:         BulletinBoardCancel()";
#else
static char     defaultTranslations[];
static char     defaultAccelerators[];
#endif

static XtActionsRec actionsList[] =
{
    {"Enter", (XtActionProc) _XmManagerEnter},	/* Motif 1.0 */
    {"FocusIn", (XtActionProc) _XmManagerFocusIn},	/* Motif 1.0 */
    {"Arm", (XtActionProc) _XmGadgetArm},	/* Motif 1.0 */
    {"Activate", (XtActionProc) _XmGadgetActivate},	/* Motif 1.0 */
    {"XgInteractorHelp", (XtActionProc) _XgInteractorHelpAction},	/* Motif 1.0 */
    {"BulletinBoardCancel", (XtActionProc) _XmBulletinBoardCancel},
};


/*---------------------------------------------------*/
/* widget resources                                  */
/*---------------------------------------------------*/
static XtResource resources[] =
{
    /* xgreclass specific resources */

    {XmNoriginalMap,
	XmCOriginalMap,
	XtRString,
	sizeof(String),
	XtOffset(ReclassWidget, reclass.original_map),
	XtRString,
	""
    },

    {XmNoriginalMapset,
	XmCOriginalMapset,
	XtRString,
	sizeof(String),
	XtOffset(ReclassWidget, reclass.original_mapset),
	XtRString,
	""
    },

    {XmNnewMap,
	XmCNewMap,
	XtRString,
	sizeof(String),
	XtOffset(ReclassWidget, reclass.new_map),
	XtRString,
	""
    },

    {XmNnewMapTitle,
	XmCNewMapTitle,
	XtRString,
	sizeof(String),
	XtOffset(ReclassWidget, reclass.new_map_title),
	XtRString,
	""
    },

    {XmNnewMapset,
	XmCNewMapset,
	XtRString,
	sizeof(String),
	XtOffset(ReclassWidget, reclass.new_mapset),
	XtRString,
	""
    },

    {XmNoriginalMapLabelString,
	XmCOriginalMapLabelString,
	XmRXmString,
	sizeof(XmString),
	XtOffset(ReclassWidget, reclass.original_map_label_string),
	XmRString,
	NULL
    },

    {XmNnewMapTitleLabelString,
	XmCNewMapTitleLabelString,
	XmRXmString,
	sizeof(XmString),
	XtOffset(ReclassWidget, reclass.new_map_title_label_string),
	XmRString,
	NULL
    },

    {XmNnewMapLabelString,
	XmCNewMapLabelString,
	XmRXmString,
	sizeof(XmString),
	XtOffset(ReclassWidget, reclass.new_map_label_string),
	XmRString,
	NULL
    },

    {XmNoriginalMapListLabelString,
	XmCOriginalMapListLabelString,
	XmRXmString,
	sizeof(XmString),
	XtOffset(ReclassWidget, reclass.original_map_list_label_string),
	XmRString,
	NULL
    },

    {XmNnewMapListLabelString,
	XmCNewMapListLabelString,
	XmRXmString,
	sizeof(XmString),
	XtOffset(ReclassWidget, reclass.new_map_list_label_string),
	XmRString,
	NULL
    },

    {XmNcatValueLabelString,
	XmCCatValueLabelString,
	XmRXmString,
	sizeof(XmString),
	XtOffset(ReclassWidget, reclass.category_value_label_string),
	XmRString,
	NULL
    },

    {XmNcatNameTextLabelString,
	XmCCatNameTextLabelString,
	XmRXmString,
	sizeof(XmString),
	XtOffset(ReclassWidget, reclass.category_name_text_label_string),
	XmRString,
	NULL
    },

    {XmNreclassOnOk,
	XmCReclassOnOk,
	XmRBoolean,
	sizeof(Boolean),
	XtOffset(ReclassWidget, reclass.reclass_on_ok),
	XmRImmediate,
	(XtPointer) TRUE
    },

    {XmNreclassOnApply,
	XmCReclassOnApply,
	XmRBoolean,
	sizeof(Boolean),
	XtOffset(ReclassWidget, reclass.reclass_on_apply),
	XmRImmediate,
	(XtPointer) TRUE
    },
    /* superclass resource default overrides */

    {XmNautoUnmanage,
	XmCAutoUnmanage,
	XmRBoolean,
	sizeof(Boolean),
	XtOffset(ReclassWidget, bulletin_board.auto_unmanage),
	XmRImmediate,
	(XtPointer) TRUE
    },
    {XmNdialogType,
	XmCDialogType,
	XmRDialogType,
	sizeof(unsigned char),
	XtOffset(ReclassWidget, interactor.dialog_type),
	XmRImmediate,
	(XtPointer) XmINTERACT_WORK_AREA_TYPE
    },

    {XmNaccelerators,
	XmCAccelerators, XmRAcceleratorTable, sizeof(XtAccelerators),
	XtOffset(XmBulletinBoardWidget, core.accelerators),
	XmRString, (XtPointer) defaultAccelerators
    },

};

externaldef(xgreclassclassrec)
    ReclassClassRec reclassClassRec =
    {
	{			/* core class record        */
	     /* superclass          */ (WidgetClass) & interactorClassRec,
	     /* class_name          */ "Reclass",
	     /* widget_size         */ sizeof(ReclassRec),
	     /* class_initialize    */ ClassInitialize,
	     /* class part init     */ ClassPartInitialize,
	     /* class_inited        */ FALSE,
	     /* initialize          */ Initialize,
	     /* initialize hook     */ NULL,
	     /* realize             */ _XtInherit,
	     /* actions             */ actionsList,
	     /* num_actions         */ XtNumber(actionsList),
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
	     /* tm_table            */ defaultTranslations,
	     /* query_geometry      */ (XtGeometryHandler) _XtInherit,
	     /* display_accelerator */ NULL,
	     /* extension           */ NULL,
	},
	{			/* composite class record   */
	     /* geometry manager */ (XtGeometryHandler) _XtInherit,
	     /* set changed proc */ _XtInherit,
	     /* insert_child     */ _XtInherit,
	     /* delete_child     */ DeleteChild,
	     /* extension        */ NULL,
	},
	{			/* constraint class record  */
	     /* no additional resources  */ NULL,
	     /* num additional resources */ 0,
	     /* size of constraint rec   */ 0,
	     /* constraint_initialize    */ NULL,
	     /* constraint_destroy       */ NULL,
	     /* constraint_setvalue      */ NULL,
	     /* extension                */ NULL,
	},
	{			/* manager class record     */
	     /* translations                 */ XtInheritTranslations,
	     /* get_resources                */ NULL,
	     /* num_syn_resources            */ 0,
	     /* constraint_syn_resources     */ NULL,
	     /* num_constraint_syn_resources */ 0,
	     /* parent_process               */ XmInheritParentProcess,
	     /* extension                    */ NULL,
	},
	{			/* bulletinBoard class record */
	     /* always_install_accelerators */ TRUE,
	     /* geo_matrix_create           */ (XmGeoCreateProc) _XtInherit,
	     /* focus_moved_proc            */ NULL,
	     /* extension                   */ NULL,
	},
	{			/* interactor class record */
	     /* extension */ NULL,
	},
	{			/* reclass class record */
	     /* extension */ NULL,
	}
    };

externaldef(xgreclasswidgetclass) WidgetClass
reclassWidgetClass = (WidgetClass) & reclassClassRec;

/*
 * FUNCTION
 */
    static void
                    ClassInitialize()
{
    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
ClassPartInitialize(xgr)
    ReclassWidgetClass xgr;
#else
ClassPartInitialize(
		    ReclassWidgetClass xgr)
#endif
{

    _XmFastSubclassInit(xgr, XmRECLASS_BIT);

    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
Initialize(request, new)
    ReclassWidget   request;
    ReclassWidget   new;
#else
Initialize(
	   ReclassWidget request,
	   ReclassWidget new)
#endif
{
    Arg             args[16];
    int             numArgs;

    /*
     * Make sure we have a valid input map name / mapset combo
     */
    /*
     * Make sure we have a valid output map name / mapset combo
     */
    /*
     * Create all of the new widgets.
     */
    ReclassCreateWidgets(new);
    ReclassAdjustAndAttach(new,False);

    /*
     * Add a default callback to ok that creates the reclass file If the user
     * has specified to do that...
     */
    if (R_ReclassOnOK(new)) {
	XtAddCallback(I_OkButton(new), XmNactivateCallback,
		      XgReclassCallBack, (XtPointer) XmINTERACT_OK_BUTTON);
    }
    if (R_ReclassOnApply(new)) {
	XtAddCallback(I_ApplyButton(new), XmNactivateCallback,
		    XgReclassCallBack, (XtPointer) XmINTERACT_APPLY_BUTTON);
    }
    if (request->manager.navigation_type != XmEXCLUSIVE_TAB_GROUP)
	_XmChangeNavigationType((Widget) new, ((XtIsShell(XtParent(request))
				       ? XmSTICKY_TAB_GROUP : XmTAB_GROUP)));

    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
ReclassCreateWidgets(xgr)
    ReclassWidget   xgr;
#else
ReclassCreateWidgets(ReclassWidget xgr)
#endif
{
    Widget          form;
    Widget          browserButton;
    Arg             al[10];
    int             ac = 0;
    char            help[256];
    XmString        xms;
    int             numItems = 0;
    Dimension       width = 0;
    Dimension       height = 0;

    /*
     * We'll use a form to contain all of the reclass widget kids since the
     * interactor only allows one work area child.
     */
    ac = 0;
    form = XmCreateForm((Widget) xgr, "xgreclass_form", al, ac);
    /*
     * Add a help callback for the reclass widget work area. We can use
     * I_WorkArea because the Interactors' InsertChild routine has set
     * it...but anal retention is sometimes desireable...
     */
    if (I_WorkArea(xgr) != NULL) {
	XgAddHelpCallback(I_WorkArea(xgr), RECLASS_WIDGET_HELP, NULL);
    }
    ac = 0;
    R_OriginalMapNameFormFrame(xgr) = XmCreateFrame(form,
				      "original_map_name_rc_frame", al, ac);
    XtManageChild(R_OriginalMapNameFormFrame(xgr));

    ac = 0;
    R_NewMapTitleFormFrame(xgr) = XmCreateFrame(form,
					  "new_map_title_rc_frame", al, ac);
    XtManageChild(R_NewMapTitleFormFrame(xgr));

    ac = 0;
    R_NewMapNameFormFrame(xgr) = XmCreateFrame(form,
					   "new_map_name_rc_frame", al, ac);
    XtManageChild(R_NewMapNameFormFrame(xgr));

    ac = 0;
    R_OriginalMapNameForm(xgr) = XmCreateForm(R_OriginalMapNameFormFrame(xgr),
					      "orig_map_name_rc", al, ac);
    XtManageChild(R_OriginalMapNameForm(xgr));

    R_NewMapTitleForm(xgr) = XmCreateForm(R_NewMapTitleFormFrame(xgr),
					  "new_name_title_rc", al, ac);
    XtManageChild(R_NewMapTitleForm(xgr));

    R_NewMapNameForm(xgr) = XmCreateForm(R_NewMapNameFormFrame(xgr),
					 "new_map_name_rc", al, ac);
    XtManageChild(R_NewMapNameForm(xgr));

    ac = 0;
    if (R_OriginalMapLabelString(xgr) != NULL) {
	XtSetArg(al[ac], XmNlabelString, R_OriginalMapLabelString(xgr));
	ac++;
    }
    XtSetArg(al[ac], XmNtraversalOn, False);
    ++ac;
    R_OriginalMapLabel(xgr) = XmCreateLabelGadget(R_OriginalMapNameForm(xgr),
						  "Original Map:", al, ac);
    XtManageChild(R_OriginalMapLabel(xgr));

    if (*R_OriginalMap(xgr) == NULL) {
	Pixmap          pixmap;
	Pixel           fg, bg;
	char           *bitmapBits;
	int             bitmapWidth, bitmapHeight;

	XtVaGetValues(xgr,
		      XmNforeground, &fg,
		      XmNbackground, &bg,
		      NULL);

	pixmap = XCreatePixmapFromBitmapData(XtDisplay(xgr),
					  RootWindowOfScreen(XtScreen(xgr)),
			  raster_bm_bits, raster_bm_width, raster_bm_height,
			       fg, bg, DefaultDepthOfScreen(XtScreen(xgr)));

	browserButton = XtVaCreateManagedWidget("parm_widget",
						xmPushButtonWidgetClass,
						R_OriginalMapNameForm(xgr),
						XmNlabelType, XmPIXMAP,
						XmNlabelPixmap, pixmap,
				      XmNnavigationType, XmSTICKY_TAB_GROUP,
						NULL);
	XtAddCallback(browserButton, XmNactivateCallback,
		      BrowserCallBack, (XtPointer) xgr);


    }
    ac = 0;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    R_OriginalMapText(xgr) = XmCreateTextField(R_OriginalMapNameForm(xgr),
					       "orig_map_text", al, ac);
    XtManageChild(R_OriginalMapText(xgr));
    if (*R_OriginalMap(xgr)) {
	char            name[512];
	char            mapset[512];
	if (_XgNameIsFullyQualified(R_OriginalMap(xgr), name, mapset)) {
	    R_OriginalMap(xgr) = XtNewString(name);
	    R_OriginalMapset(xgr) = XtNewString(mapset);
	} 
	XmTextFieldInsert(R_OriginalMapText(xgr), 0, R_OriginalMap(xgr));
	if (*R_OriginalMapset(xgr)) {
	    XmTextFieldInsert(R_OriginalMapText(xgr),
			 XmTextFieldGetLastPosition(R_OriginalMapText(xgr)),
			      "@");
	    XmTextFieldInsert(R_OriginalMapText(xgr),
			 XmTextFieldGetLastPosition(R_OriginalMapText(xgr)),
			      R_OriginalMapset(xgr));
	}
    }
    XtAddCallback(R_OriginalMapText(xgr), XmNactivateCallback,
		  OriginalMapTextCallBack, (XtPointer) xgr);

    XtVaSetValues(R_OriginalMapLabel(xgr),
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNtopAttachment, XmATTACH_FORM,
		  NULL);

    if (*R_OriginalMap(xgr) == NULL) {
	XtVaSetValues(browserButton,
		      XmNleftAttachment, XmATTACH_WIDGET,
		      XmNleftWidget, R_OriginalMapLabel(xgr),
		      XmNtopAttachment, XmATTACH_FORM,
		      NULL);
	XtVaSetValues(R_OriginalMapText(xgr),
		      XmNleftAttachment, XmATTACH_WIDGET,
		      XmNleftWidget, browserButton,
		      XmNrightAttachment, XmATTACH_FORM,
		      XmNtopAttachment, XmATTACH_FORM,
		      NULL);
    } else {
	XtVaSetValues(R_OriginalMapText(xgr),
		      XmNleftAttachment, XmATTACH_WIDGET,
		      XmNleftWidget, R_OriginalMapLabel(xgr),
		      XmNrightAttachment, XmATTACH_FORM,
		      XmNtopAttachment, XmATTACH_FORM,
		      NULL);
    }


    ac = 0;
    if (R_NewMapTitleLabelString(xgr) != NULL) {
	XtSetArg(al[ac], XmNlabelString, R_NewMapTitleLabelString(xgr));
	ac++;
    }
    XtSetArg(al[ac], XmNtraversalOn, False);
    ++ac;
    R_NewMapTitleLabel(xgr) = XmCreateLabelGadget(R_NewMapTitleForm(xgr),
						  "New Map Title:", al, ac);
    XtManageChild(R_NewMapTitleLabel(xgr));

    ac = 0;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    R_NewMapTitleText(xgr) = XmCreateTextField(R_NewMapTitleForm(xgr),
					       "new_map_title_text", al, ac);
    XtManageChild(R_NewMapTitleText(xgr));

    if (*R_NewMapTitle(xgr)) {
	XmTextFieldInsert(R_NewMapTitleText(xgr), 0, R_NewMapTitle(xgr));
    } else {
	time_t          now = time(NULL);
	char           *ptr;
	char           *timePtr;

	timePtr = XtNewString(ctime(&now));
	ptr = strcpy(XtMalloc(strlen(timePtr) + 1),
		     StripNewLine(timePtr));
	if (*R_NewMap(xgr)) {
	  char name[512];
	  char mapset[512];

	    if (_XgNameIsFullyQualified(R_NewMap(xgr), name, mapset)) {
		R_NewMap(xgr) = XtNewString(name);
	    } 
	    XmTextFieldInsert(R_NewMapTitleText(xgr), 0, R_NewMap(xgr));
	    if (*R_NewMapset(xgr)) {
		XmTextFieldInsert(R_NewMapTitleText(xgr),
			 XmTextFieldGetLastPosition(R_NewMapTitleText(xgr)),
				  "@");
		XmTextFieldInsert(R_NewMapTitleText(xgr),
			 XmTextFieldGetLastPosition(R_NewMapTitleText(xgr)),
				  R_NewMapset(xgr));
	    }
	    XmTextFieldInsert(R_NewMapTitleText(xgr),
			 XmTextFieldGetLastPosition(R_NewMapTitleText(xgr)),
			      " - ");
	}
	XmTextFieldInsert(R_NewMapTitleText(xgr),
			  XmTextFieldGetLastPosition(R_NewMapTitleText(xgr)),
			  "Created by ");
	XmTextFieldInsert(R_NewMapTitleText(xgr),
			  XmTextFieldGetLastPosition(R_NewMapTitleText(xgr)),
			  _XgGetUserName());
	XmTextFieldInsert(R_NewMapTitleText(xgr),
			  XmTextFieldGetLastPosition(R_NewMapTitleText(xgr)),
			  " on ");
	XmTextFieldInsert(R_NewMapTitleText(xgr),
			  XmTextFieldGetLastPosition(R_NewMapTitleText(xgr)),
			  ptr);
	XtFree(timePtr);
	XtFree(ptr);
    }
    XtAddCallback(R_NewMapTitleText(xgr), XmNactivateCallback,
		  NewMapTitleTextCallBack, (XtPointer) xgr);
    XtAddCallback(R_NewMapTitleText(xgr), XmNvalueChangedCallback,
		  NewMapTitleTextCallBack, (XtPointer) xgr);

    XtVaSetValues(R_NewMapTitleLabel(xgr),
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNtopAttachment, XmATTACH_FORM,
		  NULL);

    XtVaSetValues(R_NewMapTitleText(xgr),
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, R_NewMapTitleLabel(xgr),
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNtopAttachment, XmATTACH_FORM,
		  NULL);

    ac = 0;
    if (R_NewMapLabelString(xgr) != NULL) {
	XtSetArg(al[ac], XmNlabelString, R_NewMapLabelString(xgr));
	ac++;
    }
    XtSetArg(al[ac], XmNtraversalOn, False);
    ++ac;
    R_NewMapLabel(xgr) = XmCreateLabelGadget(R_NewMapNameForm(xgr),
					     "New Map:", al, ac);
    XtManageChild(R_NewMapLabel(xgr));

    ac = 0;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    R_NewMapText(xgr) = XmCreateTextField(R_NewMapNameForm(xgr),
					  "new_map_text", al, ac);
    XtManageChild(R_NewMapText(xgr));
    if (*R_NewMap(xgr)) {
	char            name[512];
	char            mapset[512];
	if (_XgNameIsFullyQualified(R_NewMap(xgr), name, mapset)) {
	    R_NewMap(xgr) = XtNewString(name);
	} 
	XmTextFieldInsert(R_NewMapText(xgr), 0, R_NewMap(xgr));
	if (*R_NewMapset(xgr)) {
	    XmTextFieldInsert(R_NewMapText(xgr),
			      XmTextFieldGetLastPosition(R_NewMapText(xgr)),
			      "@");
	    XmTextFieldInsert(R_NewMapText(xgr),
			      XmTextFieldGetLastPosition(R_NewMapText(xgr)),
			      R_NewMapset(xgr));
	}
    }
    XtAddCallback(R_NewMapText(xgr), XmNactivateCallback,
		  NewMapTextCallBack, (XtPointer) xgr);
#ifdef WHYISTHISHERE
    XtAddCallback(R_NewMapText(xgr), XmNvalueChangedCallback,
		  NewMapTextCallBack, (XtPointer) xgr);
#endif

    XtVaSetValues(R_NewMapLabel(xgr),
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNtopAttachment, XmATTACH_FORM,
		  NULL);

    XtVaSetValues(R_NewMapText(xgr),
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, R_NewMapLabel(xgr),
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNtopAttachment, XmATTACH_FORM,
		  NULL);

    ac = 0;
    XtSetArg(al[ac], XmNentryAlignment, XmALIGNMENT_BEGINNING);
    ac++;
    XtSetArg(al[ac], XmNisAligned, True);
    ac++;
    XtSetArg(al[ac], XmNorientation, XmVERTICAL);
    ac++;
    XtSetArg(al[ac], XmNpacking, XmPACK_TIGHT);
    ac++;
    XtSetArg(al[ac], XmNspacing, 0);
    ac++;
    XtSetArg(al[ac], XmNadjustLast, False);
    ac++;
    R_OriginalMapListRC(xgr) = XmCreateRowColumn(form, "orig_list_rc", al, ac);
    XtManageChild(R_OriginalMapListRC(xgr));

    R_NewMapListRC(xgr) = XmCreateRowColumn(form, "new_list_rc", al, ac);
    XtManageChild(R_NewMapListRC(xgr));

    ac = 0;
    if (R_OriginalMapListLabelString(xgr) != NULL) {
	XtSetArg(al[ac], XmNlabelString, R_OriginalMapListLabelString(xgr));
	ac++;
    }
    XtSetArg(al[ac], XmNtraversalOn, False);
    ++ac;
    R_OriginalMapListLabel(xgr) = XmCreateLabelGadget(R_OriginalMapListRC(xgr),
					    "Original Categories:", al, ac);
    XtManageChild(R_OriginalMapListLabel(xgr));

    numItems = CreateOriginalMapListItems(xgr);
    ac = 0;
    if (numItems > 0) {
	XtSetArg(al[ac], XmNitems, R_OriginalMapListItems(xgr));
	ac++;
	XtSetArg(al[ac], XmNitemCount, R_OriginalMapListItemCount(xgr));
	ac++;
    }
    XtSetArg(al[ac], XmNvisibleItemCount, 10);
    ac++;
    XtSetArg(al[ac], XmNstringDirection, I_StringDirection(xgr));
    ac++;
    XtSetArg(al[ac], XmNlistSizePolicy, XmCONSTANT);
    ac++;
    XtSetArg(al[ac], XmNselectionPolicy, XmEXTENDED_SELECT);
    ac++;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED);
    ac++;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    R_OriginalMapList(xgr) = XmCreateScrolledList(R_OriginalMapListRC(xgr),
						  "orig_list", al, ac);
    XtManageChild(R_OriginalMapList(xgr));
    XgAddHelpCallBackFromFile(R_OriginalMapList(xgr), "list_ext_help");

    XtVaGetValues(XtParent(R_OriginalMapList(xgr)),
		  XmNwidth, &width, NULL);
    XtVaGetValues(XtParent(R_OriginalMapList(xgr)),
		  XmNheight, &height, NULL);


    ac = 0;
    R_CategoryValueIncrementFrame(xgr) = XmCreateFrame(form,
				  "category_value_increment_frame", al, ac);
    XtManageChild(R_CategoryValueIncrementFrame(xgr));

    ac = 0;
    R_CategoryValueIncrementForm(xgr) = XmCreateForm(
					 R_CategoryValueIncrementFrame(xgr),
				   "category_value_increment_form", al, ac);
    XtManageChild(R_CategoryValueIncrementForm(xgr));

    ac = 0;
    if (R_CategoryValueLabelString(xgr) != NULL) {
	XtSetArg(al[ac], XmNlabelString, R_CategoryValueLabelString(xgr));
	ac++;
    }
    XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
    ac++;
    XtSetArg(al[ac], XmNtraversalOn, False);
    ++ac;
    R_CategoryValueLabel(xgr) = XmCreateLabelGadget(
		R_CategoryValueIncrementForm(xgr), "New Category:", al, ac);
    XtManageChild(R_CategoryValueLabel(xgr));

    ac = 0;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    XtSetArg(al[ac], XmNarrowDirection, XmARROW_UP);
    ++ac;
    R_CategoryValueIncrementUp(xgr) = XmCreateArrowButtonGadget(
		 R_CategoryValueIncrementForm(xgr), "increment_up", al, ac);
    XtManageChild(R_CategoryValueIncrementUp(xgr));
    XtAddCallback(R_CategoryValueIncrementUp(xgr), XmNactivateCallback,
		  IncrementCallBack, (XtPointer) xgr);

    ac = 0;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    XtSetArg(al[ac], XmNarrowDirection, XmARROW_DOWN);
    ++ac;
    R_CategoryValueIncrementDown(xgr) = XmCreateArrowButtonGadget(
	       R_CategoryValueIncrementForm(xgr), "increment_down", al, ac);
    XtManageChild(R_CategoryValueIncrementDown(xgr));
    XtAddCallback(R_CategoryValueIncrementDown(xgr), XmNactivateCallback,
		  IncrementCallBack, (XtPointer) xgr);

    ac = 0;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    R_CategoryValueIncrementText(xgr) = XmCreateTextField(
					  R_CategoryValueIncrementForm(xgr),
				   "category_value_increment_text", al, ac);
    XtManageChild(R_CategoryValueIncrementText(xgr));
    R_CategoryValueIncrementValue(xgr) = 0;
    XmTextFieldInsert(R_CategoryValueIncrementText(xgr), 0, "0");
    XtAddCallback(R_CategoryValueIncrementText(xgr), XmNactivateCallback,
		  IncrementTextCallBack, (XtPointer) xgr);
    XtAddCallback(R_CategoryValueIncrementText(xgr), XmNvalueChangedCallback,
		  IncrementTextCallBack, (XtPointer) xgr);
    XtAddCallback(R_CategoryValueIncrementText(xgr), XmNmodifyVerifyCallback,
		  IncrementTextVerifyCallBack, (XtPointer) xgr);

    ac = 0;
    if (R_CategoryNameTextLabelString(xgr) != NULL) {
	XtSetArg(al[ac], XmNlabelString, R_CategoryNameTextLabelString(xgr));
	ac++;
    }
    XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
    ac++;
    XtSetArg(al[ac], XmNtraversalOn, False);
    ++ac;
    R_CategoryNameTextLabel(xgr) = XmCreateLabelGadget(
		    R_CategoryValueIncrementForm(xgr), "New Category Name:",
						       al, ac);
    XtManageChild(R_CategoryNameTextLabel(xgr));

    ac = 0;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ac++;
    R_CategoryNameText(xgr) = XmCreateTextField(
					  R_CategoryValueIncrementForm(xgr),
					      "category_name_text", al, ac);
    XtManageChild(R_CategoryNameText(xgr));

    XtVaSetValues(R_CategoryValueLabel(xgr),
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XtVaSetValues(R_CategoryValueIncrementUp(xgr),
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, R_CategoryValueLabel(xgr),
		  XmNleftAttachment, XmATTACH_FORM,
		  NULL);
    XtVaSetValues(R_CategoryValueIncrementDown(xgr),
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, R_CategoryValueIncrementUp(xgr),
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget, R_CategoryNameTextLabel(xgr),
		  NULL);
    XtVaSetValues(R_CategoryValueIncrementText(xgr),
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, R_CategoryValueLabel(xgr),
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, R_CategoryValueIncrementUp(xgr),
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget, R_CategoryNameTextLabel(xgr),
		  NULL);
    XtVaSetValues(R_CategoryNameTextLabel(xgr),
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget, R_CategoryNameText(xgr),
		  NULL);
    XtVaSetValues(R_CategoryNameText(xgr),
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);

    ac = 0;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    R_CategoryGroupButton(xgr) =
	XmCreatePushButtonGadget(form, "Group Selected Categories ->", al, ac);
    XtManageChild(R_CategoryGroupButton(xgr));
    XtAddCallback(R_CategoryGroupButton(xgr), XmNactivateCallback,
		  GroupCallBack, (XtPointer) xgr);

    ac = 0;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    R_CategoryUngroupButton(xgr) =
	XmCreatePushButtonGadget(form, "<- Ungroup Selected Category", al, ac);
    XtManageChild(R_CategoryUngroupButton(xgr));
    XtAddCallback(R_CategoryUngroupButton(xgr), XmNactivateCallback,
		  UngroupCallBack, (XtPointer) xgr);

    R_CategoryStretchLabel(xgr) =
	XtVaCreateManagedWidget("reclass_stretch_label", xmLabelWidgetClass, form,
				XmNlabelString, XmStringCreateSimple(""),
				XmNtraversalOn, False,
				NULL);

    ac = 0;
    if (R_NewMapListLabelString(xgr) != NULL) {
	XtSetArg(al[ac], XmNlabelString, R_NewMapListLabelString(xgr));
	ac++;
    }
    XtSetArg(al[ac], XmNtraversalOn, False);
    ++ac;
    R_NewMapListLabel(xgr) = XmCreateLabelGadget(R_NewMapListRC(xgr),
						 "New Categories:",
						 al, ac);
    XtManageChild(R_NewMapListLabel(xgr));

    ac = 0;
    XtSetArg(al[ac], XmNwidth, width);
    ac++;
    XtSetArg(al[ac], XmNheight, height);
    ac++;
    XtSetArg(al[ac], XmNvisibleItemCount, 10);
    ac++;
    XtSetArg(al[ac], XmNstringDirection, I_StringDirection(xgr));
    ac++;
    XtSetArg(al[ac], XmNlistSizePolicy, XmCONSTANT);
    ac++;
    XtSetArg(al[ac], XmNselectionPolicy, XmSINGLE_SELECT);
    ac++;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED);
    ac++;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    R_NewMapList(xgr) = XmCreateScrolledList(R_NewMapListRC(xgr),
					     "new_list", al, ac);
    XtManageChild(R_NewMapList(xgr));


    XtManageChild(form);

    return;
}

/*
 * FUNCTION
 */
static int
#ifdef _NO_PROTO
CreateOriginalMapListItems(xgr)
    ReclassWidget   xgr;
#else
CreateOriginalMapListItems(ReclassWidget xgr)
#endif
{
    String          name = R_OriginalMap(xgr);
    String          mapset = R_OriginalMapset(xgr);
    int             numItems = 0;
    CELL            i;
    struct Range    range;
    CELL            min, max;

    /* name not set, return 0 */
    if (*name == NULL) {
	return 0;
    }
    if (*mapset == NULL) {
	mapset = G_mapset();
    }
    if (G_read_cats(name, mapset, &R_OriginalCategories(xgr)) < 0) {
	fprintf(stderr, "Read cats failed\n");
	exit(1);
    }
    if (G_read_range(name, mapset, &R_OriginalRange(xgr)) < 0) {
	fprintf(stderr, "Read cats failed\n");
	exit(1);
    }
    G_get_range_min_max(&R_OriginalRange(xgr), &min, &max);

    numItems = max - min + 1;
    R_OriginalMapListItems(xgr) = (XmStringTable) XtCalloc(
						numItems, sizeof(XmString));

    for (i = min; i <= max; i++) {
	char            buf[2048];	/* ...because Michael said so... */
	char           *label;

	label = G_get_cat((CELL) i, &R_OriginalCategories(xgr));

	if (*label == NULL)
	    sprintf(buf, "%d: no-label", i);
	else
	    sprintf(buf, "%d: %s", i, label);
	R_OriginalMapListItems(xgr)[i - min] = XmStringCreateSimple(buf);
    }

    R_OriginalMapListItemCount(xgr) = numItems;

    return (numItems);
}


/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
ReclassAdjustAndAttach(xgr, attach)
    ReclassWidget   xgr;
    Boolean         attach;
#else
ReclassAdjustAndAttach(ReclassWidget xgr, Boolean attach)
#endif
{

    XtVaSetValues(R_OriginalMapNameFormFrame(xgr),
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XtVaSetValues(R_NewMapTitleFormFrame(xgr),
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, R_OriginalMapNameFormFrame(xgr),
		  XmNtopOffset, BB_MarginHeight(xgr),
		  XmNleftAttachment, XmATTACH_FORM,
		  NULL);
    XtVaSetValues(R_NewMapNameFormFrame(xgr),
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, R_NewMapTitleFormFrame(xgr),
		  XmNtopOffset, BB_MarginHeight(xgr),
		  XmNleftAttachment, XmATTACH_FORM,
		  NULL);
    XtVaSetValues(R_OriginalMapListRC(xgr),
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, R_NewMapNameFormFrame(xgr),
		  XmNtopOffset, BB_MarginHeight(xgr),
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    XtVaSetValues(R_NewMapListRC(xgr),
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, R_NewMapNameFormFrame(xgr),
		  XmNtopOffset, BB_MarginHeight(xgr),
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    XtVaSetValues(R_CategoryValueIncrementFrame(xgr),
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, R_NewMapNameFormFrame(xgr),
		  XmNtopOffset, BB_MarginHeight(xgr),
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, R_OriginalMapListRC(xgr),
		  XmNleftOffset, BB_MarginWidth(xgr),
		  XmNrightAttachment, XmATTACH_WIDGET,
		  XmNrightWidget, R_NewMapListRC(xgr),
		  XmNrightOffset, BB_MarginWidth(xgr),
		  NULL);
    XtVaSetValues(R_CategoryGroupButton(xgr),
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, R_OriginalMapListRC(xgr),
		  XmNleftOffset, BB_MarginWidth(xgr),
		  XmNrightAttachment, XmATTACH_WIDGET,
		  XmNrightWidget, R_NewMapListRC(xgr),
		  XmNrightOffset, BB_MarginWidth(xgr),
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, R_CategoryValueIncrementFrame(xgr),
		  XmNtopOffset, BB_MarginHeight(xgr),
		  NULL);
    XtVaSetValues(R_CategoryUngroupButton(xgr),
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, R_OriginalMapListRC(xgr),
		  XmNleftOffset, BB_MarginWidth(xgr),
		  XmNrightAttachment, XmATTACH_WIDGET,
		  XmNrightWidget, R_NewMapListRC(xgr),
		  XmNrightOffset, BB_MarginWidth(xgr),
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, R_CategoryGroupButton(xgr),
		  XmNtopOffset, BB_MarginHeight(xgr),
		  NULL);
    XtVaSetValues(R_CategoryStretchLabel(xgr),
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, R_CategoryUngroupButton(xgr),
		  XmNtopOffset, BB_MarginWidth(xgr),
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, R_OriginalMapListRC(xgr),
		  XmNleftOffset, BB_MarginWidth(xgr),
		  XmNrightAttachment, XmATTACH_WIDGET,
		  XmNrightWidget, R_NewMapListRC(xgr),
		  XmNrightOffset, BB_MarginWidth(xgr),
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
}

static void
#ifdef _NO_PROTO
NewMapTitleTextCallBack(w, client_data, call_data)
    Widget          w;
    XtPointer       client_data;
    XtPointer       call_data;
#else
NewMapTitleTextCallBack(
			Widget w, XtPointer client_data, XtPointer call_data)
#endif
{
    ReclassWidget   xgr = (ReclassWidget) client_data;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
    char           *text;

    /* title must be less then RECORD_LEN characters in length */
    text = XmTextFieldGetString(R_NewMapTitleText(xgr));
    if (strlen(text) > (RECORD_LEN - 1)) {
	char            buf[256];

	text[RECORD_LEN - 1] = '\0';
	R_NewMapTitleTextString(xgr) = text;
	sprintf(buf, "Title must be less than %d characters long.", RECORD_LEN);
	XgWarningDialog((Widget) xgr, buf);
	return;
    }
    R_NewMapTitleTextString(xgr) = text;

}

static void
#ifdef _NO_PROTO
NewMapTextCallBack(w, client_data, call_data)
    Widget          w;
    XtPointer       client_data;
    XtPointer       call_data;
#else
NewMapTextCallBack(
		   Widget w, XtPointer client_data, XtPointer call_data)
#endif
{
    ReclassWidget   xgr = (ReclassWidget) client_data;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
    char           *text;
    char            name[512];
    char            mapset[512];
    char            newmap[512];

    text = XmTextFieldGetString(R_NewMapText(xgr));
    if (G_legal_filename(text) < 0) {
	XgWarningDialog((Widget) xgr, "Illegal filename.");
	XtRemoveCallback(R_NewMapText(xgr), XmNvalueChangedCallback,
			 NewMapTextCallBack, (XtPointer) xgr);
	XmTextFieldSetString(R_NewMapText(xgr), "");
	*R_NewMap(xgr) = NULL;
	XtAddCallback(R_NewMapText(xgr), XmNvalueChangedCallback,
		      NewMapTextCallBack, (XtPointer) xgr);
    }
    if (_XgNameIsFullyQualified(text, name, mapset)) {
	R_NewMap(xgr) = XtNewString(name);
	if (G__mapset_permissions(mapset) != 1) {
	    XgWarningDialog((Widget) xgr, "You do not have write permission in that mapset.  Your current mapset will be used instead.");
	    R_NewMapset(xgr) = XtNewString(G_mapset());
	    sprintf(newmap, "%s@%s", name, G_mapset());
	    XmTextFieldSetString(R_NewMapText(xgr), newmap);
	} else {
	    R_NewMapset(xgr) = XtNewString(mapset);
	}
    } else {
	R_NewMap(xgr) = XtNewString(text);
	R_NewMapset(xgr) = XtNewString(G_mapset());
    }

}

static void
#ifdef _NO_PROTO
OriginalMapTextCallBack(w, client_data, call_data)
    Widget          w;
    XtPointer       client_data;
    XtPointer       call_data;
#else
OriginalMapTextCallBack(
			Widget w, XtPointer client_data, XtPointer call_data)
#endif
{
    ReclassWidget   xgr = (ReclassWidget) client_data;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
    char           *text;
    char            name[512];
    char            mapset[512];
    char            *nmapset;
    Arg             al[10];
    int             ac = 0;
    int             numItems = 0;

    text = XmTextFieldGetString(R_OriginalMapText(xgr));

    /* remove this callback if value changed */
    if (cbs->reason == XmCR_VALUE_CHANGED)
	XtRemoveCallback(R_OriginalMapText(xgr), XmNvalueChangedCallback,
			 OriginalMapTextCallBack, (XtPointer) xgr);
    if (text == NULL || !strcmp(text, "")) {
	*R_OriginalMap(xgr) = 0;
	*R_OriginalMapset(xgr) = 0;
	return;
    }
    if (_XgNameIsFullyQualified(text, name, mapset)) {
	R_OriginalMap(xgr) = XtNewString(name);
	R_OriginalMapset(xgr) = XtNewString(mapset);
    } else {
	R_OriginalMap(xgr) = XtNewString(text);
        strcpy(name,text);
	R_OriginalMapset(xgr) = NULL;
    }
    if ( R_OriginalMapset(xgr) != NULL) {
	if ((nmapset = G_find_file("cell", name, mapset)) == NULL) {
	    XgWarningDialog((Widget) xgr, "No such raster map!!!");
	    XmTextFieldSetString(R_OriginalMapText(xgr), "");
	    return;
	}
    } else {
	if ((nmapset = G_find_file("cell", name, "")) == NULL) {
	    XgWarningDialog((Widget) xgr, "No such raster map!!!");
	    XmTextFieldSetString(R_OriginalMapText(xgr), "");
	    return;
	}
    }
    if ( R_OriginalMapset(xgr) == NULL ) {
        char buf[1024];

	sprintf(buf,"%s@%s", name, nmapset);
        R_OriginalMapset(xgr) = XtNewString(nmapset);
	XmTextFieldSetString(R_OriginalMapText(xgr),buf);
    }
    if (R_OriginalMapListItems(xgr) != NULL) {
	FreeOriginalMapListItems(xgr);
    }
    numItems = CreateOriginalMapListItems(xgr);
    ac = 0;
    if (numItems > 0) {
	XtSetArg(al[ac], XmNitems, R_OriginalMapListItems(xgr));
	ac++;
	XtSetArg(al[ac], XmNitemCount, R_OriginalMapListItemCount(xgr));
	ac++;
    }
    XtSetValues(R_OriginalMapList(xgr), al, ac);

}

static void
#ifdef _NO_PROTO
FreeOriginalMapListItems(xgr)
    ReclassWidget   xgr;
#else
FreeOriginalMapListItems(ReclassWidget xgr)
#endif
{
    int             i;
    int             min, max;

    G_get_range_min_max(&R_OriginalRange(xgr), &min, &max);

    for (i = min; i <= max; i++) {
	XmStringFree(R_OriginalMapListItems(xgr)[i - min]);
    }
    XtFree(R_OriginalMapListItems(xgr));
}

static void
#ifdef _NO_PROTO
BrowserOkCallBack(w, client_data, call_data)
    Widget          w;
    XtPointer       client_data;
    XtPointer       call_data;
#else
BrowserOkCallBack(
		  Widget w, XtPointer client_data, XtPointer call_data)
#endif
{
    ReclassWidget   xgr = (ReclassWidget) client_data;
    char           *result;
    XmString        xms;

    XtVaGetValues(w, XmNresultString, &xms, NULL);
    XmStringGetLtoR(xms, XmSTRING_DEFAULT_CHARSET, &result);
    /*
     * add this callback to do the checking and invoke the category list
     * update
     */
    XtAddCallback(R_OriginalMapText(xgr), XmNvalueChangedCallback,
		  OriginalMapTextCallBack, (XtPointer) xgr);
    if (result != NULL) {
	XtVaSetValues(R_OriginalMapText(xgr), XmNvalue, result, NULL);
    } else {
	XtVaSetValues(R_OriginalMapText(xgr), XmNvalue, "", NULL);
    }
}

static void
#ifdef _NO_PROTO
BrowserCallBack(w, client_data, call_data)
    Widget          w;
    XtPointer       client_data;
    XtPointer       call_data;
#else
BrowserCallBack(
		Widget w, XtPointer client_data, XtPointer call_data)
#endif
{
    ReclassWidget   xgr = (ReclassWidget) client_data;
    Widget          xgb;
    Arg             al[10];
    int             ac = 0;
    XmString        xms;
    XmString        xms2;

    xms = XmStringCreateSimple("Please select a raster map");
    if (*R_OriginalMapset(xgr) == NULL) {
	xms2 = XmStringCreateSimple(G_mapset());
    } else {
	xms2 = XmStringCreateSimple(R_OriginalMapset(xgr));
    }
    XtSetArg(al[ac], XmNnumLists, 1);
    ac++;
    XtSetArg(al[ac], XmNenableWorkAreaStretch, True);
    ac++;
    XtSetArg(al[ac], XmNpromptLabelString, xms);
    ac++;
    XtSetArg(al[ac], XmNselMode, XG_SINGLE_SELECT);
    ac++;
    XtSetArg(al[ac], XmNbrowseMode, XG_RASTER);
    ac++;
    XtSetArg(al[ac], XmNinitialMapset1, xms2);
    ac++;
    xgb = XgCreateBrowserDialog((Widget) xgr, "XGRASS Raster Browser", al, ac);
    XtManageChild(XgInteractorGetChild(xgb, XmINTERACT_PROMPT_LABEL));
    XtUnmanageChild(XgInteractorGetChild(xgb, XmINTERACT_APPLY_BUTTON));
    XtAddCallback(xgb, XmNokCallback, BrowserOkCallBack, (XtPointer) xgr);
    XmStringFree(xms);
    XmStringFree(xms2);
    XtManageChild(xgb);
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
    ReclassWidget   xgr = (ReclassWidget) client_data;
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *) call_data;
    int             i;

    for (i = 0; i < cbs->text->length; i++) {
	if ((cbs->startPos + i) == 0) {
	    if (((!isdigit(*(cbs->text->ptr + i)))) && (*(cbs->text->ptr + i)) != '-') {
		cbs->doit = False;
		XBell(XtDisplay(xgr), 0);
		return;
	    }
	} else {
	    if (!isdigit(*(cbs->text->ptr + i))) {
		cbs->doit = False;
		XBell(XtDisplay(xgr), 0);
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
    ReclassWidget   xgr = (ReclassWidget) client_data;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
    char           *text;
    int             value;

    text = XmTextFieldGetString(R_CategoryValueIncrementText(xgr));
    value = atoi(text);
    R_CategoryValueIncrementValue(xgr) = value;
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
    ReclassWidget   xgr = (ReclassWidget) client_data;
    char           *text;
    int             value;
    char            new_text[10];


    text = XmTextFieldGetString(R_CategoryValueIncrementText(xgr));
    value = atoi(text);
    if (w == R_CategoryValueIncrementUp(xgr)) {
	value++;
    } else {
	value--;
    }
    R_CategoryValueIncrementValue(xgr) = value;
    sprintf(new_text, "%d", value);
    XmTextFieldSetString(R_CategoryValueIncrementText(xgr), new_text);
}

static void
#ifdef _NO_PROTO
GroupCallBack(w, client_data, call_data)
    Widget          w;
    XtPointer       client_data;
    XtPointer       call_data;
#else
GroupCallBack(Widget w, XtPointer client_data, XtPointer call_data)
#endif
{
    ReclassWidget   xgr = (ReclassWidget) client_data;
    XgReclassRule   rule;
    XmStringTable   items;
    XmString        xms;
    char           *newCatText;
    char           *newLabel;
    int             count;
    int             i;
    char            buf[256];

    XgDoHourGlass(XtParent(xgr));


    XtVaGetValues(R_OriginalMapList(xgr),
		  XmNselectedItemCount, &count,
		  XmNselectedItems, &items,
		  NULL);

    if (count == 0) {
	XgUndoHourGlass(XtParent(xgr));
	XgWarningDialog((Widget) xgr, "No items are currently selected!!");
	return;
    }
    newLabel = XmTextFieldGetString(R_CategoryNameText(xgr));

    rule = AllocReclassRuleStruct();

    if (*newLabel == NULL) {
	newLabel = "no-label";
	rule->newLabel = NULL;
    } else {
	rule->newLabel = newLabel;
    }

    rule->numOriginalCats = count;
    newCatText = XmTextFieldGetString(R_CategoryValueIncrementText(xgr));
    rule->newCat = (CELL) atoi(newCatText);
    if (FindReclassRule(xgr, rule)) {
	XgUndoHourGlass(XtParent(xgr));
	sprintf(buf, "New category [%d] already exists.", rule->newCat);
	XgWarningDialog((Widget) xgr, buf);
	XtFree(rule);
	return;
    }
    rule->originalCats = (CELL *) XtCalloc(count, sizeof(CELL));
    for (i = 0; i < count; i++) {
	char           *text, *ptr;
	XmStringGetLtoR(items[i], XmSTRING_DEFAULT_CHARSET, &text);
	ptr = text;
	while (*ptr != ':')
	    ptr++;
	*ptr = '\0';
	rule->originalCats[i] = (CELL) atoi(text);
	*ptr = ':';
    }
    LinkReclassRule(xgr, rule);
    sprintf(buf, "%d: %s", rule->newCat, newLabel);
    xms = XmStringCreateSimple(buf);
    XmListDeleteItems(R_OriginalMapList(xgr), items, count);
    XmListAddItem(R_NewMapList(xgr), xms, 0);
    XgSortListItems(R_NewMapList(xgr), 1);
    XmListSelectItem(R_NewMapList(xgr), xms, False);
    XgUndoHourGlass(XtParent(xgr));
}

static          XgReclassRule
AllocReclassRuleStruct()
{
    XgReclassRule   rule = (XgReclassRule) XtMalloc(sizeof(XgReclassRuleRec));

    bzero((char *) rule, sizeof(XgReclassRuleRec));
    return (rule);
}

static          XgReclassRule
#ifdef _NO_PROTO
FindReclassRule(xgr, rule)
    ReclassWidget   xgr;
    XgReclassRule   rule;
#else
FindReclassRule(
		ReclassWidget xgr,
		XgReclassRule rule)
#endif
{
    XgReclassRule   ptr = R_NewReclass(xgr);

    while (ptr) {
	if (rule->newCat == ptr->newCat)
	    return ptr;
	ptr = ptr->next;
    }
    return NULL;
}

static void
#ifdef _NO_PROTO
LinkReclassRule(xgr, rule)
    ReclassWidget   xgr;
    XgReclassRule   rule;
#else
LinkReclassRule(
		ReclassWidget xgr,
		XgReclassRule rule)
#endif
{
    XgReclassRule   ptr = R_NewReclass(xgr);

    if (ptr == NULL) {
	R_NewReclass(xgr) = rule;
	return;
    }
    while (ptr->next) {
	ptr = ptr->next;
    }
    ptr->next = rule;
    return;
}

static void
#ifdef _NO_PROTO
FreeReclassRuleList(rule)
    XgReclassRule   rule;
#else
FreeReclassRuleList(XgReclassRule rule)
#endif
{
    if (rule->next)
	FreeReclassRuleList(rule->next);
    XtFree(rule);
    rule = NULL;
}


static void
#ifdef _NO_PROTO
UngroupCallBack(w, client_data, call_data)
    Widget          w;
    XtPointer       client_data;
    XtPointer       call_data;
#else
UngroupCallBack(Widget w, XtPointer client_data, XtPointer call_data)
#endif
{
    ReclassWidget   xgr = (ReclassWidget) client_data;
    XgReclassRuleRec dummy;
    XgReclassRule   rule;
    XgReclassRule   rPtr, tPtr;
    XmStringTable   items;
    XmStringTable   newItems;
    int             min, max;
    int             newCat;
    int             count;
    int             i;
    char           *text, *ptr;
    char            buf[256];

    XgDoHourGlass(XtParent(xgr));

    XtVaGetValues(R_NewMapList(xgr),
		  XmNselectedItemCount, &count,
		  XmNselectedItems, &items,
		  NULL);

    if (count == 0) {
	XgUndoHourGlass(XtParent(xgr));
	XgWarningDialog((Widget) xgr, "No items are currently selected!!");
	return;
    }
    /* locate the selected item in the rule list */

    XmStringGetLtoR(items[0], XmSTRING_DEFAULT_CHARSET, &text);
    ptr = text;
    while (*ptr != ':')
	ptr++;
    *ptr = '\0';
    newCat = (CELL) atoi(text);
    *ptr = ':';
    dummy.newCat = newCat;
    rule = FindReclassRule(xgr, &dummy);

    /* unlink the rule */
    rPtr = R_NewReclass(xgr);
    if (rPtr == rule) {
	if (rPtr->next == NULL)
	    R_NewReclass(xgr) = NULL;
	else {
	    R_NewReclass(xgr) = rPtr->next;
	}
    } else {
	Boolean         first = True;

	tPtr = rPtr;
	while (rPtr != rule) {
	    if (first) {
		first = False;
	    } else {
		tPtr = tPtr->next;
	    }
	    rPtr = rPtr->next;
	}
	/* at this point tPtr points to the item before rule */
	tPtr->next = rule->next;
    }

    /* re-create the original categories */
    G_get_range_min_max(&R_OriginalRange(xgr), &min, &max);

    newItems = (XmStringTable) XtCalloc(rule->numOriginalCats, sizeof(XmString));
    /* loop thru the original cats in this reclass rule */
    for (i = 0; i < rule->numOriginalCats; i++) {
	char            buf[2048];	/* ...because Michael said so... */
	char           *label;

	/* reconstruct the label */
	label = G_get_cat((CELL) rule->originalCats[i],
			  &R_OriginalCategories(xgr));

	if (*label == NULL)
	    sprintf(buf, "%d: no-label", rule->originalCats[i]);
	else
	    sprintf(buf, "%d: %s", rule->originalCats[i], label);
	/* and the item */
	newItems[i] = XmStringCreateSimple(buf);
    }
    /* add them */
    XmListAddItems(R_OriginalMapList(xgr), newItems, rule->numOriginalCats,0);
    /* sort them */
    XgSortListItems(R_OriginalMapList(xgr), 1);
    /* select them and free on the way */
    XtVaSetValues(R_OriginalMapList(xgr), XmNselectedItems, newItems, 
        XmNselectedItemCount, rule->numOriginalCats, NULL);
    for (i = 0; i < rule->numOriginalCats; i++) {
	XmStringFree(newItems[i]);
    }
    XtFree(newItems);
    /* delete the new list item */
    XmListDeleteItem(R_NewMapList(xgr), items[0]);
    /* reset the new cat label and number text fields */
    sprintf(buf, "%d", rule->newCat);
    XmTextFieldSetString(R_CategoryValueIncrementText(xgr), buf);
    if ( rule->newLabel == NULL || *rule->newLabel == NULL ) {
	XmTextFieldSetString(R_CategoryNameText(xgr), "no-label");
    } else {
	XmTextFieldSetString(R_CategoryNameText(xgr), rule->newLabel);
    }
    /* free the rule */
    XtFree(rule);
    XgUndoHourGlass(XtParent(xgr));
}

static void
#ifdef _NO_PROTO
RemoveHelpCallbacks(xgr)
    ReclassWidget   xgr;
#else
RemoveHelpCallbacks(ReclassWidget xgr)
#endif
{
    /*
     * KAB FIX XtRemoveAllCallbacks(P_Scale1Label(xgr), XmNhelpCallback);
     * XtRemoveAllCallbacks(P_Scale2Label(xgr), XmNhelpCallback);
     * XtRemoveAllCallbacks(P_Scale3Label(xgr), XmNhelpCallback);
     * 
     * XtRemoveAllCallbacks(P_Scale1(xgr), XmNhelpCallback);
     * XtRemoveAllCallbacks(P_Scale2(xgr), XmNhelpCallback);
     * XtRemoveAllCallbacks(P_Scale3(xgr), XmNhelpCallback);
     * 
     * XtRemoveAllCallbacks(P_Scale1Text(xgr), XmNhelpCallback);
     * XtRemoveAllCallbacks(P_Scale2Text(xgr), XmNhelpCallback);
     * XtRemoveAllCallbacks(P_Scale3Text(xgr), XmNhelpCallback);
     */
}

static void
#ifdef _NO_PROTO
AddHelpCallbacks(which, xgr)
    int             which;
    ReclassWidget   xgr;
#else
AddHelpCallbacks(int which, ReclassWidget xgr)
#endif
{
    Widget          widget1;
    Widget          widget2;
    Widget          widget3;
    char            help[1024];
    char            thelp[1024];
    /*
     * KAB FIX XgAddHelpCallback(widget1, "This label indicates that the",
     * component[which - 1][P_ScaleType(xgr)], "component of the color will
     * be modified by ", "manipulating the scale or text area widgets to the
     * right.", GENERIC_LABEL_HELP, NULL);
     * 
     * XgAddHelpCallback(widget2, "Manipulating this scale widget will modify
     * the", component[which - 1][P_ScaleType(xgr)], "component of the
     * color.", GENERIC_SCALE_HELP, NULL);
     * 
     * XgAddHelpCallback(widget3, "Changing this text field will modify the",
     * component[which - 1][P_ScaleType(xgr)], "component of the color.",
     * (P_ScaleType(xgr) == XgHSV ? "You should enter a decimal value in the
     * range 0.0 to 1.0." : "You should enter an integer value in the range 0
     * to 255."), "You must hit the return key to activate the change.",
     * NULL);
     */
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
Destroy(xgr)
    ReclassWidget   xgr;
#else
Destroy(
	ReclassWidget xgr)
#endif
{
    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
DeleteChild(w)
    Widget          w;
#else
DeleteChild(
	    Widget w)
#endif
{
    ReclassWidget   xgr;

    if (XtIsRectObj(w)) {
	xgr = (ReclassWidget) XtParent(XtParent(w));

	/*
	 * KAB FIX if (w == P_Scale1Label(xgr)) { P_Scale1Label(xgr) = NULL;
	 * } else { if (w == P_Scale1(xgr)) { P_Scale1(xgr) = NULL; } else {
	 * if (w == P_Scale1Text(xgr)) { P_Scale1Text(xgr) = NULL; } else {
	 * if (w == P_Scale2Label(xgr)) { P_Scale2Label(xgr) = NULL; } else {
	 * if (w == P_Scale2(xgr)) { P_Scale2(xgr) = NULL; } else { if (w ==
	 * P_Scale2Text(xgr)) { P_Scale2Text(xgr) = NULL; } else { if (w ==
	 * P_Scale3Label(xgr)) { P_Scale3Label(xgr) = NULL; } else { if (w ==
	 * P_Scale3(xgr)) { P_Scale3(xgr) = NULL; } else { if (w ==
	 * P_Scale3Text(xgr)) { P_Scale3Text(xgr) = NULL; } else { if (w ==
	 * P_DisplayArea(xgr)) { P_DisplayArea(xgr) = NULL; } } } } } } } } }
	 * }
	 */
    }
    (*((InteractorWidgetClass) interactorWidgetClass)
     ->composite_class.delete_child) (w);
    return;
}

/*
 * FUNCTION
 */
static          Boolean
#ifdef _NO_PROTO
SetValues(current, request, new)
    ReclassWidget   current;
    ReclassWidget   request;
    ReclassWidget   new;
#else
SetValues(
	  ReclassWidget current,
	  ReclassWidget request,
	  ReclassWidget new)
#endif
{
    Arg             args[10];
    int             n;
    String          newString;

    BB_InSetValues(new) = TRUE;

    /*
     * KAB if (P_Scale1LabelString(current) != P_Scale1LabelString(new)) {
     * 
     * P_Scale1LabelString(new) = XmStringCopy(P_Scale1LabelString(new));
     * 
     * n = 0; XtSetArg(args[n], XmNlabelString, P_Scale1LabelString(new)); n++;
     * XtSetArg(args[n], XmNlabelType, XmSTRING); n++;
     * XtSetValues(P_Scale1Label(new), args, n); }
     */
    BB_InSetValues(new) = FALSE;

    if (XtClass(new) == reclassWidgetClass) {
	_XmBulletinBoardSizeUpdate((XmBulletinBoardWidget) new);
    }
    return (FALSE);
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
XgReclassCallBack(wid, client_data, call_data)
    Widget          wid;
    XtPointer       client_data;
    XtPointer       call_data;
#else
XgReclassCallBack(
		  Widget wid,
		  XtPointer client_data,
		  XtPointer call_data)
#endif
{
    XmAnyCallbackStruct *callback = (XmAnyCallbackStruct *) call_data;
    InteractorCallbackStruct callbackStruct;
    unsigned char   which_button = (unsigned char) client_data;
    ReclassWidget   xgr = (ReclassWidget) XtParent(wid);

    callbackStruct.value = (XmString) which_button;
    callbackStruct.event = callback->event;

    switch ((int) which_button) {
    case XmINTERACT_OK_BUTTON:
	/* we're here because ReclassOnOk is True, so do it... */
	ExecuteReclass(xgr);
	break;
    case XmINTERACT_APPLY_BUTTON:
	/* we're here because ReclassOnApply is True, so do it... */
	ExecuteReclass(xgr);
	break;
    case XmINTERACT_HELP_BUTTON:
	break;
    case XmINTERACT_CANCEL_BUTTON:
	break;
    }

    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
ExecuteReclass(xgr)
    ReclassWidget   xgr;
#else
ExecuteReclass(ReclassWidget xgr)
#endif
{
    Widget          qd;
    Arg             al[10];
    int             ac = 0;
    int             count;
    XmString        xms;
    char           *text;

    XtVaGetValues(R_OriginalMapList(xgr), XmNitemCount, &count, NULL);
    text = XmTextFieldGetString(R_OriginalMapText(xgr));
    if (*text == NULL) {
	XgWarningDialog((Widget) xgr, "No orignal map name.");
	return;
    }
    text = XmTextFieldGetString(R_NewMapText(xgr));
    if (*text == NULL) {
	XgWarningDialog((Widget) xgr, "No new map name.");
	return;
    }
    if (count) {
	xms = XmStringCreateSimple(
		   "Not all original items are regrouped, OK to continue?");
	XtSetArg(al[ac], XmNmessageString, xms);
	ac++;
	qd = XmCreateQuestionDialog((Widget) xgr, "question_dialog", al, ac);
	XtAddCallback(qd, XmNokCallback, FinishExecute, (XtPointer) xgr);
	XtManageChild(qd);
	XtUnmanageChild(XmMessageBoxGetChild(qd, XmDIALOG_HELP_BUTTON));
	XmStringFree(xms);
    } else {
	FinishExecute(NULL, (XtPointer) xgr, NULL);
    }
}

static char    *
#ifdef _NO_PROTO
StripNewLine(s)
    char           *s;
#else
StripNewLine(char *s)
#endif
{
    char           *ptr = s;

    while (*ptr) {
	if (*ptr == '\n')
	    *ptr = '\0';
	ptr++;
    }
    return s;
}

static void
#ifdef _NO_PROTO
FinishExecute(wid, client_data, call_data)
    Widget          wid;
    XtPointer       client_data;
    XtPointer       call_data;
#else
FinishExecute(
	      Widget wid,
	      XtPointer client_data,
	      XtPointer call_data)
#endif
{
    ReclassWidget   xgr = (ReclassWidget) client_data;
    XgReclassRule   ptr = R_NewReclass(xgr);
    int             i;
    FILE           *fp;
    char            buf[1024];
    char           *text;
    char           *timePtr;
    char           *tmpfile  = XtNewString(tmpnam(NULL));
    time_t          now = time(NULL);

    text = XmTextFieldGetString(R_NewMapTitleText(xgr));
    if (*text == NULL) {
	timePtr = XtNewString(ctime(&now));
	text = XtMalloc(strlen("Reclass of ") +
			strlen(XmTextFieldGetString(R_NewMapText(xgr))) +
			strlen(". Created by") +
			strlen(_XgGetUserName()) +
			strlen(" on ") +
			strlen(timePtr) + 1);
	strcpy(text, "Reclass of ");
	strcat(text, XmTextFieldGetString(R_NewMapText(xgr)));
	strcat(text, ". Created by");
	strcat(text, _XgGetUserName());
	strcat(text, " on ");
	strcat(text, StripNewLine(timePtr));
    }
    if ((fp = fopen(tmpfile, "w")) == NULL) {
	fprintf(stderr, "Can't open reclass rules file %s.\n",tmpfile);
	return;
    }
    while (ptr) {
	for (i = 0; i < ptr->numOriginalCats; i++) {
	    fprintf(fp, "%d ", ptr->originalCats[i]);
	}
	if (ptr->newLabel) {
	    fprintf(fp, "= %d %s\n", ptr->newCat, ptr->newLabel);
	} else {
	    fprintf(fp, "= %d\n", ptr->newCat);
	}
	ptr = ptr->next;
    }
    fprintf(fp, "end\n");
    fclose(fp);

    sprintf(buf, "r.reclass input=%s output=%s title=\"%s\" < %s",
	    XmTextFieldGetString(R_OriginalMapText(xgr)),
	    XmTextFieldGetString(R_NewMapText(xgr)),
	    text, tmpfile);

    XgSystem(XtParent(xgr), buf, True, NULL, 0);
    XgSetCommandString(XtDisplay(xgr),XgGetMenuWindow(XtDisplay(xgr)),buf);
    XFlush(XtDisplay(xgr));

    XmListDeleteAllItems(R_NewMapList(xgr));
    FreeReclassRuleList(R_NewReclass(xgr));
    R_NewReclass(xgr) = NULL;

    XtFree(text);
}

/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgReclassGetChild(xgr, which)
    Widget          xgr;	/* Reclass widget  */
    unsigned char   which;	/* Which child          */
#else
XgReclassGetChild(
		  Widget xgr,	/* Reclass widget  */
		  unsigned char which)	/* Which child          */
#endif
{
    Widget          child = NULL;

    /*
     * KAB FIX switch (which) { case XgPIXEL_DISPLAY_AREA: { child =
     * P_DisplayArea(xgr); break; } case XgPIXEL_SCALE1_LABEL: { child =
     * P_Scale1Label(xgr); break; } case XgPIXEL_SCALE1: { child =
     * P_Scale1(xgr); break; } case XgPIXEL_SCALE1_TEXT: { child =
     * P_Scale1Text(xgr); break; } case XgPIXEL_SCALE2_LABEL: { child =
     * P_Scale2Label(xgr); break; } case XgPIXEL_SCALE2: { child =
     * P_Scale2(xgr); break; } case XgPIXEL_SCALE2_TEXT: { child =
     * P_Scale2Text(xgr); break; } case XgPIXEL_SCALE3_LABEL: { child =
     * P_Scale3Label(xgr); break; } case XgPIXEL_SCALE3: { child =
     * P_Scale3(xgr); break; } case XgPIXEL_SCALE3_TEXT: { child =
     * P_Scale3Text(xgr); break; } default: { child =
     * XgInteractorGetChild(xgr, which); break; } }
     */
    return (child);
}

/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgCreateReclass(p, name, args, n)
    Widget          p;		/* parent widget   */
    String          name;	/* widget name     */
    ArgList         args;	/* arg list        */
    Cardinal        n;		/* arg count       */
#else
XgCreateReclass(
		Widget p,	/* parent widget   */
		String name,	/* widget name     */
		ArgList args,	/* arg list        */
		Cardinal n)	/* arg count       */
#endif
{
    return (XtCreateWidget(name, reclassWidgetClass, p, args, n));
}

/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgCreateReclassDialog(ds_p, name, xgr_args, xgr_n)
    Widget          ds_p;	/* parent for shell    */
    String          name;	/* widget name         */
    ArgList         xgr_args;	/* arglist for xgr      */
    Cardinal        xgr_n;	/* argcount for xgr     */
#else
XgCreateReclassDialog(
		      Widget ds_p,	/* parent for shell    */
		      String name,	/* widget name         */
		      ArgList xgr_args,	/* arglist for xgr      */
		      Cardinal xgr_n)	/* argcount for xgr     */
#endif
{
    Widget          xgr;	/* new xgr widget      */
    Widget          ds;		/* DialogShell         */
    Arg             ds_args[10];/* arglist for shell  */
    char           *ds_name;

    /*
     * Create DialogShell parent.
     */
    ds_name = XtCalloc(strlen(name) + 1, sizeof(char));
    strcpy(ds_name, name);

    XtSetArg(ds_args[0], XmNallowShellResize, True);
    ds = XmCreateDialogShell(ds_p, ds_name, ds_args, 1);

    XtFree(ds_name);

    /*
     * Create Reclass widget.
     */
    xgr = XtCreateWidget(name, reclassWidgetClass, ds,
			 xgr_args, xgr_n);
    XtAddCallback(xgr, XmNdestroyCallback, _XmDestroyParentCallback, NULL);

    return (xgr);
}

#ifdef _NO_PROTO
_XgNameIsFullyQualified(text, name, mapset)
char *text, *name, *mapset;
#else
_XgNameIsFullyQualified(char *text, char *name, char *mapset)
#endif
{
    char **tokens = _XgTokenize(text, "@");

    if ( _XgNumberOfTokens(tokens) == 2 ) {
	strcpy(name, tokens[0]);
	strcpy(mapset, tokens[1]);
	return 1;
    }
    return 0;
}
