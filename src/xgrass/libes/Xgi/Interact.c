static char                     rcsid[] = "@(#)XGRASS $Id: Interact.c,v 0.0 1992/05/05 14:56:02 sink Exp sink $";
/*
 * File: Interact.c
 * 
 * Desc: Interactor widget implementation file
 * 
 * Auth: Kurt Buehler
 * 
 * Date: Wed Jan  8 14:44:41 CST 1992
 * 
 * Modification History:
 * 
 * 
 */

#include "InteractP.h"
#include "Help.h"

#include <X11/cursorfont.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/RowColumn.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/DialogS.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/Text.h>

#include <stdio.h>

typedef struct {
    XmKidGeometry list_label;       /* Pointer to list_label geometry */
    XmKidGeometry list_text_label;  /* Pointer to list_text_label geometry */
} I_GeoExtensionRec, *I_GeoExtension;


#define WARN_INTERACT_TYPE "Incorrect dialog type."
#define WARN_INTERACT_TYPE_CHANGE "Dialog type cannot be modified."
#define WARN_WORK_AREA "Only one work area child allowed."
#define WARN_CHILD_TYPE "Invalid child type."
#define WARN_HELP_ITEMS "Help items can be accessed only at creation time."
#define WARN_HELP_NON_NEGATIVE "Help item count must be non-negative."
#define WARN_HELP_NULL "Help items or labels NULL."

/*
 * Internal functions:
 */
#ifdef _NO_PROTO

static void                     ClassInitialize();
static void                     ClassPartInitialize();
static void                     Initialize();
static void                     InsertChild();
static void                     DeleteChild();
static void                     _XmDialogTypeDefault();
static void                     XgInteractorCallBack();
static void                     PromptTextCallBack();
static void                     TextCallBack();
static void                     ListCallBack();
static Boolean                  SetValues();
static void                     Destroy();
static void                     CopyItems();
static void                     CopyItemLabels();
static void                     ClearItems();
static void                     ClearItemLabels();
static XgHelpStruct             *AllocHelpStruct();
static void                     FreeHelpStruct();
static void                     _XgListTextLabelFix();
static void                     _XgListTextFix();
static void                     _XgListLabelFix();
static void                     _XgListFix();
static void                     XgDoHelpWidgetHelp();

#else                           /* _NO_PROTO */

static void                     ClassInitialize();
static void                     ClassPartInitialize(InteractorWidgetClass class);
static void                     Initialize(InteractorWidget request, InteractorWidget new);
static void                     InsertChild(Widget child);
static void                     DeleteChild(Widget child);
static void                     _XmDialogTypeDefault(Widget widget, int offset, XrmValue * value);
static void                     XgInteractorCallBack(Widget w, caddr_t client_data, caddr_t call_data);
static void                     PromptTextCallBack(Widget w, caddr_t client_data, caddr_t call_data);
static void                     TextCallBack(Widget w, caddr_t client_data, caddr_t call_data);
static void                     ListCallBack(Widget w, caddr_t client_data, caddr_t call_data);
static Boolean                  SetValues(InteractorWidget current, InteractorWidget request, InteractorWidget new);
static void                     Destroy(InteractorWidget xgi);
static void                     CopyItems(InteractorWidget xgi);
static void                     CopyItemLabels(InteractorWidget xgi);
static void                     ClearItems(InteractorWidget xgi);
static void                     ClearItemLabels(InteractorWidget xgi);
static XgHelpStruct             *AllocHelpStruct(InteractorWidget xgi);
static void                     FreeHelpStruct(XgHelpStruct *ptr);
static void                     _XgListTextLabelFix(XmGeoMatrix geoSpec, int action, XmGeoRowLayout layoutPtr, XmKidGeometry rowPtr);
static void                     _XgListTextFix(XmGeoMatrix geoSpec, int action, XmGeoRowLayout layoutPtr, XmKidGeometry rowPtr);
static void                     _XgListLabelFix(XmGeoMatrix geoSpec, int action, XmGeoRowLayout layoutPtr, XmKidGeometry rowPtr);
static void                     _XgListFix(XmGeoMatrix geoSpec, int action, XmGeoRowLayout layoutPtr, XmKidGeometry rowPtr);
static void                     XgDoHelpWidgetHelp(Widget w, char *s,XmString j);
#endif                          /* _NO_PROTO */

#ifndef MCCABE
static char                     defaultTranslations[] =
"<Key>osfSelect:         ManagerGadgetSelect()\n\
        <Key>osfHelp:           XgInteractorHelp()\n\
        <Key>osfCancel:         BulletinBoardCancel()\n\
        <Key>osfActivate:       InteractorActivate()\n\
	~Shift ~Meta ~Alt <Key>Return:  InteractorActivate()\n\
        ~Shift ~Meta ~Alt <Key>space:   ManagerGadgetSelect()\n\
        <Key>:                  ManagerGadgetKeyInput()\n\
        <BtnMotion>:    ManagerGadgetButtonMotion()\n\
        <Btn1Down>:     ManagerGadgetArm()\n\
        <Btn1Down>,<Btn1Up>:    ManagerGadgetActivate()\n\
        <Btn1Up>:       ManagerGadgetActivate()\n\
        <Btn1Down>(2+): ManagerGadgetMultiArm()\n\
        <Btn1Up>(2+):   ManagerGadgetMultiActivate()";

static char                     defaultAccelerators[] =
"\043override\n\
        <Key>osfCancel:         BulletinBoardCancel()\n\
	~Shift ~Meta ~Alt <Key>Return:  InteractorActivate()\n\
        <Key>osfActivate:       InteractorActivate()";
#else
static char                     defaultTranslations[];
static char                     defaultAccelerators[];
#endif

static XtActionsRec             actionsList[] =
{
    {"Enter", (XtActionProc) _XmManagerEnter},
    {"FocusIn", (XtActionProc) _XmManagerFocusIn},
    {"Arm", (XtActionProc) _XmGadgetArm},
    {"Activate", (XtActionProc) _XmGadgetActivate},
    {"Help", (XtActionProc) _XgInteractorHelpAction},
    {"Return", (XtActionProc) _XgInteractorActivate },
    {"XgInteractorHelp", (XtActionProc) _XgInteractorHelpAction},
    {"BulletinBoardCancel", (XtActionProc) _XmBulletinBoardCancel},
    {"InteractorActivate", (XtActionProc) _XgInteractorActivate },
};



/*
 * Resource definitions for Interactor
 */

static XmSyntheticResource      syn_resources[] =
{
    {XmNokLabelString,
        sizeof(XmString),
        XtOffset(InteractorWidget, interactor.ok_label_string),
        _XgInteractorGetOkLabelString,
        NULL
    },

    {XmNapplyLabelString,
        sizeof(XmString),
        XtOffset(InteractorWidget, interactor.apply_label_string),
        _XgInteractorGetApplyLabelString,
        NULL
    },

    {XmNcancelLabelString,
        sizeof(XmString),
        XtOffset(InteractorWidget, interactor.cancel_label_string),
        _XgInteractorGetCancelLabelString,
        NULL
    },

    {XmNhelpLabelString,
        sizeof(XmString),
        XtOffset(InteractorWidget, interactor.help_label_string),
        _XgInteractorGetHelpLabelString,
        NULL
    },

    {XmNpromptLabelString,
        sizeof(XmString),
        XtOffset(InteractorWidget, interactor.prompt_label_string),
        _XgInteractorGetPromptLabelString,
        NULL
    },

    {XmNlistLabelString,
        sizeof(XmString),
        XtOffset(InteractorWidget, interactor.list_label_string),
        _XgInteractorGetListLabelString,
        NULL
    },

    {XmNtextColumns,
        sizeof(short),
        XtOffset(InteractorWidget, interactor.text_columns),
        _XgInteractorGetTextColumns,
        NULL
    },

    {XmNtextString,
        sizeof(XmString),
        XtOffset(InteractorWidget, interactor.text_string),
        _XgInteractorGetTextString,
        NULL
    },

    {XmNlistTextLabelString,
        sizeof(XmString),
        XtOffset(InteractorWidget, interactor.list_text_label_string),
        _XgInteractorGetListTextLabelString,
        NULL
    },

    {XmNlistTextColumns,
        sizeof(short),
        XtOffset(InteractorWidget, interactor.text_columns),
        _XgInteractorGetListTextColumns,
        NULL
    },

    {XmNlistTextString,
        sizeof(XmString),
        XtOffset(InteractorWidget, interactor.text_string),
        _XgInteractorGetListTextString,
        NULL
    },

    {XmNlistItems,
        sizeof(XmString *),
        XtOffset(InteractorWidget, interactor.list_items),
        _XgInteractorGetListItems,
        NULL
    },

    {XmNlistItemCount,
        sizeof(int),
        XtOffset(InteractorWidget, interactor.list_item_count),
        _XgInteractorGetListItemCount,
        NULL
    },

    {XmNlistVisibleItemCount,
        sizeof(int),
        XtOffset(InteractorWidget, interactor.list_visible_item_count),
        _XgInteractorGetListVisibleItemCount,
        NULL
    },

};




static XtResource               resources[] =
{

    {XmNhelpItemLabels,
        XmCHelpItemLabels, XmRXmStringTable, sizeof(XmStringTable),
        XtOffset(InteractorWidget, interactor.help_item_labels),
        XmRStringTable, NULL
    },

    {XmNhelpItemCount,
        XmCHelpItemCount, XtRInt, sizeof(int),
        XtOffset(InteractorWidget, interactor.help_item_count),
        XtRImmediate, (XtPointer) 0
    },

    {XmNhelpItems,
        XmCHelpItems, XtRStringArray, sizeof(String *),
        XtOffset(InteractorWidget, interactor.help_items),
        XtRPointer, NULL
    },

    {XmNlistLabelString,
        XmCListLabelString, XmRXmString, sizeof(XmString),
        XtOffset(InteractorWidget, interactor.list_label_string),
        XmRString, NULL
    },

    {XmNlistTextLabelString,
        XmCListTextLabelString, XmRXmString, sizeof(XmString),
        XtOffset(InteractorWidget, interactor.list_text_label_string),
        XmRString, NULL
    },

    {XmNpromptLabelString,
        XmCPromptLabelString, XmRXmString, sizeof(XmString),
        XtOffset(InteractorWidget, interactor.prompt_label_string),
        XmRString, NULL
    },

    {XmNtextColumns,
        XmCColumns, XmRShort, sizeof(short),
        XtOffset(InteractorWidget, interactor.text_columns),
        XmRImmediate, (caddr_t) 20
    },

    {XmNtextString,
        XmCTextString, XmRXmString, sizeof(XmString),
        XtOffset(InteractorWidget, interactor.text_string),
        XmRImmediate, (caddr_t) UNSPECIFIED
    },

    {XmNlistTextColumns,
        XmCColumns, XmRShort, sizeof(short),
        XtOffset(InteractorWidget, interactor.list_text_columns),
        XmRImmediate, (caddr_t) 20
    },

    {XmNlistTextString,
        XmCListTextString, XmRXmString, sizeof(XmString),
        XtOffset(InteractorWidget, interactor.list_text_string),
        XmRImmediate, (caddr_t) UNSPECIFIED
    },

    {XmNlistItems,
        XmCItems, XmRXmStringTable, sizeof(XmStringTable),
        XtOffset(InteractorWidget, interactor.list_items),
        XmRImmediate, NULL
    },

    {XmNlistItemCount,
        XmCItemCount, XmRInt, sizeof(int),
        XtOffset(InteractorWidget, interactor.list_item_count),
        XmRImmediate, (caddr_t) UNSPECIFIED
    },

    {XmNlistVisibleItemCount,
        XmCVisibleItemCount, XmRInt, sizeof(int),
        XtOffset(InteractorWidget, interactor.list_visible_item_count),
        XmRImmediate, (caddr_t) 8
    },

    {XmNokLabelString,
        XmCOkLabelString, XmRXmString, sizeof(XmString),
        XtOffset(InteractorWidget, interactor.ok_label_string),
        XmRString, NULL
    },

    {XmNapplyLabelString,
        XmCApplyLabelString, XmRXmString, sizeof(XmString),
        XtOffset(InteractorWidget, interactor.apply_label_string),
        XmRString, NULL
    },

    {XmNcancelLabelString,
        XmCCancelLabelString, XmRXmString, sizeof(XmString),
        XtOffset(InteractorWidget, interactor.cancel_label_string),
        XmRString, NULL
    },

    {XmNhelpLabelString,
        XmCHelpLabelString, XmRXmString, sizeof(XmString),
        XtOffset(InteractorWidget, interactor.help_label_string),
        XmRString, NULL
    },

    {XmNenableWorkAreaStretch,
        XmCEnableWorkAreaStretch, XmRBoolean, sizeof(Boolean),
        XtOffset(InteractorWidget, interactor.work_area_stretch),
        XmRImmediate, (caddr_t) False
    },

    {XmNnoMatchCallback,
	XmCCallback, XmRCallback, sizeof (XtCallbackList),
	XtOffset (InteractorWidget, interactor.no_match_callback),
	XmRImmediate, (caddr_t) NULL
    },

    {XmNmustMatch,
	XmCMustMatch, XmRBoolean, sizeof(Boolean),
	XtOffset (InteractorWidget, interactor.must_match),
	XmRImmediate, (caddr_t) False
    },

    {XmNminimizeButtons,
        XmCMinimizeButtons, XmRBoolean, sizeof(Boolean),
        XtOffset(InteractorWidget, interactor.minimize_buttons),
        XmRImmediate, (caddr_t) False
    },

    {XmNokCallback,
        XmCCallback, XmRCallback, sizeof(XtCallbackList),
        XtOffset(InteractorWidget, interactor.ok_callback),
        XmRPointer, (caddr_t) NULL
    },

    {XmNapplyCallback,
        XmCCallback, XmRCallback, sizeof(XtCallbackList),
        XtOffset(InteractorWidget, interactor.apply_callback),
        XmRPointer, (caddr_t) NULL
    },

    {XmNcancelCallback,
        XmCCallback, XmRCallback, sizeof(XtCallbackList),
        XtOffset(InteractorWidget, interactor.cancel_callback),
        XmRPointer, (caddr_t) NULL
    },

    {XmNdialogType,
        XmCDialogType, XmRDialogType, sizeof(unsigned char),
        XtOffset(InteractorWidget, interactor.dialog_type),
        XmRCallProc, (caddr_t) _XmDialogTypeDefault
    },

    {XmNaccelerators,
        XmCAccelerators, XmRAcceleratorTable, sizeof(XtAccelerators),
        XtOffset(XmBulletinBoardWidget, core.accelerators),
        XmRString, (XtPointer) defaultAccelerators
    },
};




externaldef(xginteractorclassrec) InteractorClassRec
interactorClassRec =
{
    {                           /* core class record */

         /* superclass          */ (WidgetClass) & xmBulletinBoardClassRec,
             /* class_name          */ "Interactor",
             /* widget_size         */ sizeof(InteractorRec),
             /* class_initialize    */ ClassInitialize,
             /* chained class init  */ ClassPartInitialize,
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
             /* compress enter/exit */ TRUE,
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

    {                           /* composite class record */

         /* childrens geo mgr proc */ (XtGeometryHandler) _XtInherit,
             /* set changed proc       */ _XtInherit,
             /* insert_child           */ InsertChild,
             /* delete_child           */ DeleteChild,
             /* extension              */ NULL,
    },

    {                           /* constraint class record */

         /* no additional resources  */ NULL,
             /* num additional resources */ 0,
             /* size of constraint rec   */ 0,
             /* constraint_initialize    */ NULL,
             /* constraint_destroy       */ NULL,
             /* constraint_setvalue      */ NULL,
             /* extension                */ NULL,
    },

    {                           /* manager class record */
         /* default translations   */ XtInheritTranslations,
             /* syn_resources          */ syn_resources,
             /* num_syn_resources      */ XtNumber(syn_resources),
             /* syn_cont_resources     */ NULL,
             /* num_syn_cont_resources */ 0,
             /* parent_process         */ XmInheritParentProcess,
             /* extension              */ NULL,
    },

    {                           /* bulletin board class record */
         /* always_install_accelerators */ FALSE,
             /* geo_matrix_create           */ _XgInteractorGeoMatrixCreate,
             /* focus_moved_proc            */ XtInheritFocusMovedProc,
             /* extension                   */ NULL,
    },

    {                           /* interactor class record */
         /* extension  */ NULL,
    },
};

externaldef(xginteractorwidgetclass)
    WidgetClass                     interactorWidgetClass
    = (WidgetClass) & interactorClassRec;



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
ClassPartInitialize(class)
    InteractorWidgetClass           class;
#else
ClassPartInitialize(
                    InteractorWidgetClass class)
#endif
{

    _XmFastSubclassInit(class, XmINTERACTOR_BIT);

    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
Initialize(request, new)
    InteractorWidget                request;
    InteractorWidget                new;
#else
Initialize(
           InteractorWidget request,
           InteractorWidget new)
#endif
{
    Arg                             al[5];

    I_WorkArea(new) = NULL;
    I_HelpStructList(new) = NULL;

    /*
     * Validate dialog type.
     */
    if (I_DialogType(request) != XmINTERACT_WORK_AREA_TYPE &&
            I_DialogType(request) != XmINTERACT_DIALOG_TYPE &&
            I_DialogType(request) != XmINTERACT_PROMPT_TYPE &&
            I_DialogType(request) != XmINTERACT_LIST_TYPE ) {
        _XmWarning(new, WARN_INTERACT_TYPE);
        if (request->bulletin_board.shell)
            I_DialogType(new) = XmINTERACT_DIALOG_TYPE;
        else
            I_DialogType(new) = XmINTERACT_WORK_AREA_TYPE;
    }
    /*
     * If everything seems ok, copy help items and labels. THE USER IS
     * RESPONSIBLE FOR FREEING THE ORIGINALS.
     */
    if ( I_HelpItemCount(new) < 0 ) {
	_XmWarning(new, WARN_HELP_NON_NEGATIVE);
    }
    if ( I_HelpItemCount(new) && (!I_HelpItems(new) ||
	 !I_HelpItemLabels(new))) {
        _XmWarning(new, WARN_HELP_NULL);
    }
    CopyItemLabels(new);
    CopyItems(new);

    /*
     * Create child widgets.
     */
    I_AddingIWidgets(new) = True;

    _XgInteractorCreatePromptLabel(new);
    _XgInteractorCreateText(new);
    _XgInteractorCreateListLabel(new);
    _XgInteractorCreateList(new);
    _XgInteractorCreateListTextLabel(new);
    _XgInteractorCreateListText(new);

    _XgInteractorCreateSeparator(new);
    _XgInteractorCreateOkButton(new);
    _XgInteractorCreateApplyButton(new);
    _XgInteractorCreateCancelButton(new);
    _XgInteractorCreateHelpButton(new);

    /*
    I_PromptLabelString(new) = NULL;
    I_ListLabelString(new) = NULL;
    I_ListTextLabelString(new) = NULL;
    I_OkLabelString(new) = NULL;
    I_ApplyLabelString(new) = NULL;
    I_CancelLabelString(new) = NULL;
    I_HelpLabelString(new) = NULL;
    I_TextString(new) = (XmString) UNSPECIFIED;
    */

    I_AddingIWidgets(new) = False;

    XtManageChildren(new->composite.children, new->composite.num_children);

    /*
     * Unmanage the prompt, text, list label, and list. The user must manage
     * these to use them...
     */
    if ( I_DialogType(new) != XmINTERACT_PROMPT_TYPE ) {
	if (I_PromptLabel(new) && XtIsManaged(I_PromptLabel(new)))
	    XtUnmanageChild(I_PromptLabel(new));
	if (I_Text(new) && XtIsManaged(I_Text(new)))
	    XtUnmanageChild(I_Text(new));
    }
    if ( I_DialogType(new) != XmINTERACT_LIST_TYPE ) {
	if (I_ListLabel(new) && XtIsManaged(I_ListLabel(new)))
	    XtUnmanageChild(I_ListLabel(new));
	if (I_List(new) && XtIsManaged(XtParent(I_List(new))))
	    XtUnmanageChild(XtParent(I_List(new)));
	if (I_ListTextLabel(new) && XtIsManaged(I_ListTextLabel(new)))
	    XtUnmanageChild(I_ListTextLabel(new));
	if (I_ListText(new) && XtIsManaged(I_ListText(new)))
	    XtUnmanageChild(I_ListText(new));
    }
    if ( I_DialogType(new) != XmINTERACT_WORK_AREA ) {
	if ( I_ApplyButton(new) && XtIsManaged(I_ApplyButton(new)) )
	    XtUnmanageChild(I_ApplyButton(new));
    }
    return;
}


/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
InsertChild(child)
    Widget                          child;
#else
InsertChild(
            Widget child)
#endif
{
    InteractorWidget                xgi;

    (*((XmBulletinBoardWidgetClass) xmBulletinBoardWidgetClass)
     ->composite_class.insert_child) (child);

    if (!XtIsRectObj(child)) {
        return;
    }
    xgi = (InteractorWidget) XtParent(child);

    /*
     * check if this child is to be the interactor widget's work area widget
     */
    if (!I_AddingIWidgets(xgi)) {
        if (!I_WorkArea(xgi)) {
            I_WorkArea(xgi) = child;
        } else {
            _XmWarning(xgi, WARN_WORK_AREA);
        }
    }
    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
DeleteChild(child)
    Widget                          child;
#else
DeleteChild(
            Widget child)
#endif
{
    InteractorWidget                xgi;

    if (XtIsRectObj(child)) {
        xgi = (InteractorWidget) XtParent(child);
        /*
         * Clear widget fields (BulletinBoard does default and cancel).
         */
        if (child == I_PromptLabel(xgi)) {
            I_PromptLabel(xgi) = NULL;
        } else {
            if (child == I_Text(xgi)) {
                I_Text(xgi) = NULL;
            } else {
                if (child == I_ListLabel(xgi)) {
                    I_ListLabel(xgi) = NULL;
                } else {
                    if (child == I_List(xgi)) {
                        I_List(xgi) = NULL;
		    } else {
			if (child == I_ListLabel(xgi)) {
			    I_ListLabel(xgi) = NULL;
			} else {
			    if (child == I_List(xgi)) {
				I_List(xgi) = NULL;
			    } else {
				if (child == I_WorkArea(xgi)) {
				    I_WorkArea(xgi) = NULL;
				} else {
				    if (child == I_Separator(xgi)) {
					I_Separator(xgi) = NULL;
				    } else {
					if (child == I_OkButton(xgi)) {
					    I_OkButton(xgi) = NULL;
					} else {
					    if (child == I_ApplyButton(xgi)) {
						I_ApplyButton(xgi) = NULL;
					    } else {
						if (child == I_HelpButton(xgi)) {
						    I_HelpButton(xgi) = NULL;
						}
					    }
					}
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
    (*((XmBulletinBoardWidgetClass) xmBulletinBoardWidgetClass)
     ->composite_class.delete_child) (child);
    return;
}


/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
_XmDialogTypeDefault(widget, offset, value)
    Widget                          widget;
    int                             offset;
    XrmValue                       *value;
#else
_XmDialogTypeDefault(
                     Widget widget,
                     int offset,
                     XrmValue * value)
#endif
{
    static unsigned char            type;

    /*
     * Set the default type.  To do this, we check the dialog box's parent.
     * If it is a DialogShell widget, then this is a "pop-up" dialog box, and
     * the default type is selection. Else the default type is workarea.
     */
    type = XmINTERACT_WORK_AREA_TYPE;
    if (XmIsDialogShell(XtParent(widget)))
        type = XmINTERACT_DIALOG_TYPE;
    value->addr = (caddr_t) (&type);
    return;
}


/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorCreatePromptLabel(xgi)
    InteractorWidget                xgi;
#else
_XgInteractorCreatePromptLabel(
                               InteractorWidget xgi)
#endif
{
    Arg                             al[10];
    register int                    ac = 0;

    if (I_PromptLabelString(xgi)) {
        XtSetArg(al[ac], XmNlabelString, I_PromptLabelString(xgi));
        ac++;
    }
    XtSetArg(al[ac], XmNtraversalOn, False);
    ac++;
    I_PromptLabel(xgi) =
        XmCreateLabelGadget((Widget) xgi, "interactor_prompt_label", al, ac);
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorCreateText(xgi)
    InteractorWidget                xgi;
#else
_XgInteractorCreateText(
                        InteractorWidget xgi)
#endif
{
    Arg                             al[10];
    register int                    ac = 0;
    char                           *text;

    XtSetArg(al[ac], XmNcolumns, I_TextColumns(xgi));
    ac++;
    XtSetArg(al[ac], XmNresizeWidth, False);
    ac++;
    XtSetArg(al[ac], XmNeditMode, XmSINGLE_LINE_EDIT);
    ac++;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;

    I_Text(xgi) =
        XmCreateText((Widget) xgi, "interactor_text", al, ac);

    if (I_TextString(xgi) != (XmString) UNSPECIFIED) {
        if (!XmStringGetLtoR(I_TextString(xgi), XmSTRING_DEFAULT_CHARSET,
                             &text)) {
            text = NULL;
        }
        XmTextSetString(I_Text(xgi), text);
        if (text) {
            XmTextSetCursorPosition(I_Text(xgi), strlen(text));
        }
        XtFree(text);

    }
    XtAddCallback(I_Text(xgi),XmNactivateCallback, PromptTextCallBack,
        (XtPointer) xgi) ;
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorCreateListTextLabel(xgi)
    InteractorWidget                xgi;
#else
_XgInteractorCreateListTextLabel(
                             InteractorWidget xgi)
#endif
{
    Arg                             al[10];
    register int                    ac = 0;
    XmString                        xms;
    Boolean                         def = False;

    if (I_ListTextLabelString(xgi)) {
        XtSetArg(al[ac], XmNlabelString, I_ListTextLabelString(xgi));
        ac++;
    } else {
        def = True;
	xms = XmStringCreateSimple("Selection:");
        XtSetArg(al[ac], XmNlabelString, xms);
        ac++;
    }
    XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
    ac++;
    XtSetArg(al[ac], XmNtraversalOn, False);
    ac++;
    I_ListTextLabel(xgi) =
        XmCreateLabelGadget((Widget) xgi, "interactor_list_text_label", al, ac);
    if ( def ) 
        XmStringFree(xms);
    return;
}


/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorCreateListText(xgi)
    InteractorWidget                xgi;
#else
_XgInteractorCreateListText(
                        InteractorWidget xgi)
#endif
{
    Arg                             al[10];
    register int                    ac = 0;
    char                           *text;

    XtSetArg(al[ac], XmNcolumns, I_ListTextColumns(xgi));
    ac++;
    XtSetArg(al[ac], XmNresizeWidth, False);
    ac++;
    XtSetArg(al[ac], XmNeditMode, XmSINGLE_LINE_EDIT);
    ac++;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;

    I_ListText(xgi) =
        XmCreateText((Widget) xgi, "interactor_list_text", al, ac);

    if (I_ListTextString(xgi) != (XmString) UNSPECIFIED) {
        if (!XmStringGetLtoR(I_ListTextString(xgi), XmSTRING_DEFAULT_CHARSET,
                             &text)) {
            text = NULL;
        }
        XmTextSetString(I_ListText(xgi), text);
        if (text) {
            XmTextSetCursorPosition(I_ListText(xgi), strlen(text));
        }
        XtFree(text);

    }
    XtAddCallback(I_ListText(xgi),XmNactivateCallback, TextCallBack,
        (XtPointer) xgi) ;
    return;
}


/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorCreateListLabel(xgi)
    InteractorWidget                xgi;
#else
_XgInteractorCreateListLabel(
                             InteractorWidget xgi)
#endif
{
    Arg                             al[10];
    register int                    ac = 0;
    XmString                        xms;
    Boolean                         def = False;

    if (I_ListLabelString(xgi)) {
        XtSetArg(al[ac], XmNlabelString, I_ListLabelString(xgi));
        ac++;
    } else {
        def = True;
	xms = XmStringCreateSimple("Items:");
        XtSetArg(al[ac], XmNlabelString, xms);
        ac++;
    }
    XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
    ac++;
    XtSetArg(al[ac], XmNtraversalOn, False);
    ac++;
    I_ListLabel(xgi) =
        XmCreateLabelGadget((Widget) xgi, "interactor_list_label", al, ac);
    if ( def ) 
        XmStringFree(xms);
    return;
}


/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorCreateList(xgi)
    InteractorWidget                xgi;
#else
_XgInteractorCreateList(
                        InteractorWidget xgi)
#endif
{
    Arg                             al[10];
    register int                    ac = 0;
    int *                           pos;
    int                             posCount;
    XtCallbackProc                  callbackProc;
  
    if ( I_ListItems(xgi) ) {
        XtSetArg(al[ac], XmNitems, I_ListItems(xgi)); ac++;
    }
    if ( I_ListItemCount(xgi) != UNSPECIFIED ) {
        XtSetArg(al[ac], XmNitemCount, I_ListItemCount(xgi)); ac++;
    }
    XtSetArg(al[ac], XmNvisibleItemCount, I_ListVisibleItemCount(xgi)); ac++;

    I_ListSelectedItemPosition(xgi) = 0 ;

    XtSetArg(al[ac], XmNstringDirection, I_StringDirection (xgi)) ;  ac++ ;
    XtSetArg(al[ac], XmNselectionPolicy, XmBROWSE_SELECT) ;  ac++ ;
    XtSetArg(al[ac], XmNlistSizePolicy, XmCONSTANT) ;  ac++ ;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED) ;  ac++ ;
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    I_List(xgi) =
        XmCreateScrolledList((Widget) xgi, "interactor_list", al, ac);

    if ( I_ListTextString(xgi) != (XmString) UNSPECIFIED    ) {
        if ( I_ListTextString(xgi) && XmListGetMatchPos(I_List(xgi),
                 I_ListTextString(xgi),&pos,&posCount)    ) {   
		if ( posCount ) {
                I_ListSelectedItemPosition(xgi) = pos[0] ;
                XmListSelectPos( I_List(xgi), pos[0], False) ;
	    }
            XtFree( pos) ;
	}
    }
    XtAddCallback(I_List(xgi),XmNsingleSelectionCallback, ListCallBack,
        (XtPointer) xgi) ;
    XtAddCallback(I_List(xgi),XmNbrowseSelectionCallback, ListCallBack,
        (XtPointer) xgi) ;
    XtAddCallback(I_List(xgi),XmNdefaultActionCallback, ListCallBack,
        (XtPointer) xgi) ;
    XtManageChild(I_List(xgi));

    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorCreateSeparator(xgi)
    InteractorWidget                xgi;
#else
_XgInteractorCreateSeparator(
                             InteractorWidget xgi)
#endif
{
    Arg                             al[10];
    register int                    ac = 0;

    XtSetArg(al[ac], XmNhighlightThickness, 0);
    ac++;
    I_Separator(xgi) =
        XmCreateSeparatorGadget((Widget) xgi, "interactor_separator", al, ac);
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorCreateOkButton(xgi)
    InteractorWidget                xgi;
#else
_XgInteractorCreateOkButton(
                            InteractorWidget xgi)
#endif
{

    I_OkButton(xgi) = _XgInteractorCreateButtonG((Widget) xgi,
                                                 I_OkLabelString(xgi), "OK");
    XtAddCallback(I_OkButton(xgi), XmNactivateCallback,
                  XgInteractorCallBack, XmINTERACT_OK_BUTTON);
    return;
}


/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorCreateApplyButton(xgi)
    InteractorWidget                xgi;
#else
_XgInteractorCreateApplyButton(
                               InteractorWidget xgi)
#endif
{

    I_ApplyButton(xgi) = _XgInteractorCreateButtonG((Widget) xgi,
                                          I_ApplyLabelString(xgi), "Apply");
    /*
     * Remove BulletinBoard Unmanage callback from apply and help buttons.
     */
    XtRemoveAllCallbacks(I_ApplyButton(xgi), XmNactivateCallback);
    XtAddCallback(I_ApplyButton(xgi), XmNactivateCallback,
                  XgInteractorCallBack, XmINTERACT_APPLY_BUTTON);
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorCreateCancelButton(xgi)
    InteractorWidget                xgi;
#else
_XgInteractorCreateCancelButton(
                                InteractorWidget xgi)
#endif
{

    I_CancelButton(xgi) = _XgInteractorCreateButtonG((Widget) xgi,
                                        I_CancelLabelString(xgi), "Cancel");
    XtAddCallback(I_CancelButton(xgi), XmNactivateCallback,
                  XgInteractorCallBack, XmINTERACT_CANCEL_BUTTON);
    return;
}

/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
_XgInteractorCreateButtonG(xgi, lstring, name)
    Widget                          xgi;
    XmString                        lstring;
    char                           *name;
#else
_XgInteractorCreateButtonG(Widget xgi, XmString lstring, char *name)
#endif
{
    Arg                             al[10];
    int                             ac = 0;
    Widget                          button;

    if (lstring) {
        XtSetArg(al[ac], XmNlabelString, lstring);
        ac++;
    }
    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    XtSetArg(al[ac], XmNstringDirection, BB_StringDirection(xgi));
    ac++;
    button = XmCreatePushButton/*Gadget*/((Widget) xgi, name, al, ac);
    return button;

}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorCreateHelpButton(xgi)
    InteractorWidget                xgi;
#else
_XgInteractorCreateHelpButton(
                              InteractorWidget xgi)
#endif
{
    Arg                             al[10];
    register Cardinal               ac = 0;
    XgHelpStruct                   *xgiHelp = AllocHelpStruct(xgi);
    Widget                          cascade;

    XtSetArg(al[ac], XmNnavigationType, XmSTICKY_TAB_GROUP);
    ++ac;
    I_HelpButton(xgi) = XmCreateMenuBar((Widget) xgi, "interactor_help_menubar",
                                        al, ac);
    I_HelpMenu(xgi) = XmCreatePulldownMenu(I_HelpButton(xgi),
                                           "interactor_pulldown", NULL, 0);

    if (I_HelpLabelString(xgi)) {
        char                           *text;

        if (!XmStringGetLtoR(I_HelpLabelString(xgi),
                             XmSTRING_DEFAULT_CHARSET, &text))
            text = NULL;

        XtSetArg(al[ac], XmNlabelString, I_HelpLabelString(xgi));
        ac++;
        if (text) {
            char                            accelerator[20];
            sprintf(accelerator, "Meta<Key>%c", *text);
            XtSetArg(al[ac], XmNaccelerator, accelerator);
            ac++;
            XtSetArg(al[ac], XmNmnemonic, *text);
            ac++;
        }
    } else {
        XtSetArg(al[ac], XmNaccelerator, "Meta<Key>H");
        ac++;
        XtSetArg(al[ac], XmNmnemonic, 'H');
        ac++;
    }
    XtSetArg(al[ac], XmNstringDirection, I_StringDirection(xgi));
    ac++;
    XtSetArg(al[ac], XmNsubMenuId, I_HelpMenu(xgi));
    ac++;

    I_HelpCascadeButton(xgi) = 
	XmCreateCascadeButton(I_HelpButton(xgi), "Help", al, ac);
    XtManageChild(I_HelpCascadeButton(xgi));

    _XgInteractorCreateHelpMenu(xgi);
    XtManageChild(I_HelpButton(xgi));

    return;
}

#ifdef _NO_PROTO
_XgInteractorCreateHelpMenu(xgi)
    InteractorWidget                xgi;
#else
_XgInteractorCreateHelpMenu(InteractorWidget xgi)
#endif
{
    Arg                             al[10];
    register Cardinal               ac = 0;
    Widget                          shell;
    Widget                          widget;
    XmString                       *items;
    char                          **help_text;
    int                             i;
    XgHelpStruct                   *xgiHelp;

    xgiHelp = AllocHelpStruct(xgi);

    widget = XtVaCreateManagedWidget("On Context", xmPushButtonGadgetClass,
                                     I_HelpMenu(xgi), NULL);

    xgiHelp->widget = (Widget) xgi;
    xgiHelp->text = XtNewString("$");
    XtAddCallback(widget, XmNactivateCallback,
                  _XgInteractorHelp, (XtPointer) xgiHelp);

    items = I_HelpItemLabels(xgi);
    help_text = I_HelpItems(xgi);
    for (i = 0; i < I_HelpItemCount(xgi); i++) {
        char                            popupStr[256];
        XgHelpStruct                   *xgiHelp1;

        xgiHelp1 = AllocHelpStruct(xgi);

        sprintf(popupStr, "_popup_%03d", i);
        widget = XtVaCreateManagedWidget(popupStr,
                                   xmPushButtonGadgetClass, I_HelpMenu(xgi),
                                         XmNlabelString, items[i], NULL);
        xgiHelp1->widget = (Widget) xgi;
        xgiHelp1->text = help_text[i];
        XtAddCallback(widget, XmNactivateCallback,
                      _XgInteractorHelp, (XtPointer) xgiHelp1);
    }
    return;
}


/*
 * FUNCTION
 */
XmGeoMatrix
#ifdef _NO_PROTO
_XgInteractorGeoMatrixCreate(xgi, instigator, desired)
    InteractorWidget                xgi;
    Widget                          instigator;
    XtWidgetGeometry               *desired;
#else
_XgInteractorGeoMatrixCreate(
                             InteractorWidget xgi,
                             Widget instigator,
                             XtWidgetGeometry * desired)
#endif
{
    XmGeoMatrix                     geoSpec;
    register XmGeoRowLayout         layoutPtr;
    register XmKidGeometry          boxPtr;
    XmKidGeometry                   firstButtonBox;
    Boolean isList = False;

    geoSpec = _XmGeoMatrixAlloc(I_MAX_WIDGETS_VERT, I_MAX_NUM_WIDGETS, 
	sizeof(I_GeoExtensionRec));
    geoSpec->composite = (Widget) xgi;
    geoSpec->instigator = (Widget) instigator;
    if (desired) {
        geoSpec->instig_request = *desired;
    }
    geoSpec->margin_w = BB_MarginWidth(xgi) + xgi->manager.shadow_thickness;
    geoSpec->margin_h = BB_MarginHeight(xgi) + xgi->manager.shadow_thickness;
    geoSpec->no_geo_request = _XgInteractorNoGeoRequest;

    layoutPtr = geoSpec->layouts;
    boxPtr = geoSpec->boxes;

    if (I_PromptLabel(xgi) && XtIsManaged(I_PromptLabel(xgi))
            && _XmGeoSetupKid(boxPtr, I_PromptLabel(xgi))) {
        layoutPtr->space_above = BB_MarginHeight(xgi);
        boxPtr += 2;            /* For new row, add 2. */
        ++layoutPtr;            /* For new row. */
    }
    if (I_List(xgi) && XtIsManaged(XtParent(I_List(xgi)))) {
	isList = True;
    }
    if (I_Text(xgi) && XtIsManaged(I_Text(xgi))
            && _XmGeoSetupKid(boxPtr, I_Text(xgi))) {
        layoutPtr->space_above = BB_MarginHeight(xgi);
	/*
	 * If the list is not used and the work area isn't going to stretch,
	 * then stretch the text widget.
	 */
	if ( !isList && !I_EnableWorkAreaStretch(xgi))
	    layoutPtr->stretch_height = True;
        boxPtr += 2;            /* For new row, add 2. */
        ++layoutPtr;            /* For new row. */
    }
    if (I_ListLabel(xgi) && XtIsManaged(I_ListLabel(xgi))
            && _XmGeoSetupKid(boxPtr, I_ListLabel(xgi))) {
        layoutPtr->fix_up = _XgListLabelFix;
        layoutPtr->space_above = BB_MarginHeight(xgi);
        boxPtr += 2;            /* For new row, add 2. */
        ++layoutPtr;            /* For new row. */
    }
    if ( isList && _XmGeoSetupKid(boxPtr, XtParent(I_List(xgi)))) {
        layoutPtr->fix_up = _XgListFix;
	/*
	 * The work area isn't going to stretch, so stretch the list
	 */
	if (!I_EnableWorkAreaStretch(xgi)) 
            layoutPtr->stretch_height = True;
        boxPtr += 2;            /* For new row, add 2. */
        ++layoutPtr;            /* For new row. */
    }
    if (isList && I_ListTextLabel(xgi) && XtIsManaged(I_ListTextLabel(xgi))
            && _XmGeoSetupKid(boxPtr, I_ListTextLabel(xgi))) {
        layoutPtr->fix_up = _XgListTextLabelFix;
        layoutPtr->space_above = BB_MarginHeight(xgi);
        boxPtr += 2;            /* For new row, add 2. */
        ++layoutPtr;            /* For new row. */
    }
    if ( isList && _XmGeoSetupKid(boxPtr, I_ListText(xgi))) {
        layoutPtr->fix_up = _XgListTextFix;
        boxPtr += 2;            /* For new row, add 2. */
        ++layoutPtr;            /* For new row. */
    }
    if (_XmGeoSetupKid(boxPtr, I_WorkArea(xgi))) {
        layoutPtr->space_above = BB_MarginHeight(xgi);
	/*
	 * Stretch the work area, if requested.
	 */
	if (I_EnableWorkAreaStretch(xgi)) 
            layoutPtr->stretch_height = True;
        boxPtr += 2;            /* For new row, add 2. */
        ++layoutPtr;            /* For new row. */
    }
    if (_XmGeoSetupKid(boxPtr, I_Separator(xgi))) {
        layoutPtr->fix_up = _XmSeparatorFix;
        layoutPtr->space_above = BB_MarginHeight(xgi);
        boxPtr += 2;            /* For new row, add 2. */
        ++layoutPtr;            /* For new row. */
    }
    firstButtonBox = boxPtr;
    if (_XmGeoSetupKid(boxPtr, I_OkButton(xgi))) {
        ++boxPtr;
    }
    if (_XmGeoSetupKid(boxPtr, I_ApplyButton(xgi))) {
        ++boxPtr;
    }
    if (_XmGeoSetupKid(boxPtr, I_CancelButton(xgi))) {
        ++boxPtr;
    }
    if (_XmGeoSetupKid(boxPtr, I_HelpButton(xgi))) {
        ++boxPtr;
    }
    if (boxPtr != firstButtonBox) {     /* Had at least one button. */
        layoutPtr->fill_mode = XmGEO_CENTER;
        layoutPtr->fit_mode = XmGEO_WRAP;
        layoutPtr->space_above = BB_MarginHeight(xgi);
        if (!I_MinimizeButtons(xgi)) {
            layoutPtr->even_width = 1;
        }
        layoutPtr->even_height = 1;
        ++boxPtr;
        ++layoutPtr;
    }
    layoutPtr->space_above = BB_MarginHeight(xgi);
    layoutPtr->end = TRUE;      /* Mark the last row. */
    return (geoSpec);
}

static void
#ifdef _NO_PROTO
_XgListTextLabelFix(geoSpec, action, layoutPtr, rowPtr)
    XmGeoMatrix                     geoSpec;
    int                             action;
    XmGeoRowLayout                  layoutPtr;
    XmKidGeometry                   rowPtr;
#else
_XgListTextLabelFix(
                XmGeoMatrix geoSpec,
                int action,
                XmGeoRowLayout layoutPtr,
                XmKidGeometry rowPtr)
#endif
{
    I_GeoExtension extension;

    extension = (I_GeoExtension) geoSpec->extension;
    extension->list_text_label = rowPtr;
}

static void
#ifdef _NO_PROTO
_XgListLabelFix(geoSpec, action, layoutPtr, rowPtr)
    XmGeoMatrix                     geoSpec;
    int                             action;
    XmGeoRowLayout                  layoutPtr;
    XmKidGeometry                   rowPtr;
#else
_XgListLabelFix(
                XmGeoMatrix geoSpec,
                int action,
                XmGeoRowLayout layoutPtr,
                XmKidGeometry rowPtr)
#endif
{
    I_GeoExtension extension;

    extension = (I_GeoExtension) geoSpec->extension;
    extension->list_label = rowPtr;
}

static void
#ifdef _NO_PROTO
_XgListTextFix(geoSpec, action, layoutPtr, rowPtr)
    XmGeoMatrix                     geoSpec;
    int                             action;
    XmGeoRowLayout                  layoutPtr;
    XmKidGeometry                   rowPtr;
#else
_XgListTextFix(
                XmGeoMatrix geoSpec,
                int action,
                XmGeoRowLayout layoutPtr,
                XmKidGeometry rowPtr)
#endif
{
    I_GeoExtension extension;

    extension = (I_GeoExtension) geoSpec->extension;

    switch (action) {
    case XmGET_PREFERRED_SIZE:
        if ( extension->list_text_label &&
             (extension->list_text_label->box.width < rowPtr->box.width)) {
            extension->list_text_label->box.width = rowPtr->box.width;
        }
        break;
    case XmGET_ACTUAL_SIZE:
        break;
    case XmGEO_PRE_SET:
        if ( extension->list_text_label &&
             (extension->list_text_label->box.width < rowPtr->box.width)) {
            extension->list_text_label->box.width = rowPtr->box.width;
            extension->list_text_label->box.x = rowPtr->box.x;
        }
        break;
    case XmGEO_POST_SET:
        break;
    }
}


static void
#ifdef _NO_PROTO
_XgListFix(geoSpec, action, layoutPtr, rowPtr)
    XmGeoMatrix                     geoSpec;
    int                             action;
    XmGeoRowLayout                  layoutPtr;
    XmKidGeometry                   rowPtr;
#else
_XgListFix(
                XmGeoMatrix geoSpec,
                int action,
                XmGeoRowLayout layoutPtr,
                XmKidGeometry rowPtr)
#endif
{
    I_GeoExtension extension;

    extension = (I_GeoExtension) geoSpec->extension;

    switch (action) {
    case XmGET_PREFERRED_SIZE:
	if ( extension->list_label && 
	     (extension->list_label->box.width < rowPtr->box.width)) {
	    extension->list_label->box.width = rowPtr->box.width;
	}
        break;
    case XmGET_ACTUAL_SIZE:
        break;
    case XmGEO_PRE_SET:
	if ( extension->list_label &&
	     (extension->list_label->box.width < rowPtr->box.width)) {
	    extension->list_label->box.width = rowPtr->box.width;
	    extension->list_label->box.x = rowPtr->box.x;
	}
        break;
    case XmGEO_POST_SET:
        break;
    }
}

/*
 * FUNCTION
 */
Boolean
#ifdef _NO_PROTO
_XgInteractorNoGeoRequest(geoSpec)
    XmGeoMatrix                     geoSpec;
#else
_XgInteractorNoGeoRequest(
                          XmGeoMatrix geoSpec)
#endif
{
    if (BB_InSetValues(geoSpec->composite)
            && (XtClass(geoSpec->composite) == interactorWidgetClass)) {
        return (TRUE);
    }
    return (FALSE);
}

void
#ifdef _NO_PROTO
_XgInteractorActivate( widget, event, params, numParams)
    Widget widget ;
    XEvent *event ;
    String *params ;
    Cardinal numParams ;
#else
_XgInteractorActivate(
    Widget widget,
    XEvent *event,
    String *params,
    Cardinal numParams)
#endif
{   
    InteractorWidget xgi = (InteractorWidget)widget;
    XmPrimitiveWidgetClass primitiveClass;

    if ( _XmFocusIsHere(I_OkButton(xgi))) {
	primitiveClass = (XmPrimitiveWidgetClass) XtClass(I_OkButton(xgi));
	if ( primitiveClass->primitive_class.arm_and_activate  &&
	     XtIsSensitive(I_OkButton(xgi)) ) 
	    (*(primitiveClass->primitive_class.arm_and_activate))
		(I_OkButton(xgi), event, NULL, 0);
    } else
    if ( _XmFocusIsHere(I_ApplyButton(xgi))) {
	primitiveClass = (XmPrimitiveWidgetClass) XtClass(I_ApplyButton(xgi));
	if ( primitiveClass->primitive_class.arm_and_activate  &&
	     XtIsSensitive(I_ApplyButton(xgi)) ) 
	    (*(primitiveClass->primitive_class.arm_and_activate))
		(I_ApplyButton(xgi), event, NULL, 0);
    } else
    if ( _XmFocusIsHere(I_CancelButton(xgi))) {
	primitiveClass = (XmPrimitiveWidgetClass)XtClass(I_CancelButton(xgi));
	if ( primitiveClass->primitive_class.arm_and_activate  &&
	     XtIsSensitive(I_CancelButton(xgi)) ) 
	    (*(primitiveClass->primitive_class.arm_and_activate))
		(I_CancelButton(xgi), event, NULL, 0);
    }
    return;
}

void
#ifdef _NO_PROTO
_XgInteractorHelpAction(widget, event, parms, nparms)
    Widget                          widget;
    XEvent                         *event;
    String                         *parms;
    Cardinal                       nparms;
#else
_XgInteractorHelpAction(Widget widget, XEvent * event, String * parms, Cardinal  nparms)
#endif
{
    XmAnyCallbackStruct             cb;
    Widget                          parent = widget;
    Widget                          savedWidget = widget;

    while (parent != NULL && XtClass(parent) != interactorWidgetClass)
        parent = XtParent(parent);

    if (parent == NULL)
        return;

    /*
     * Setup callback (call_data) information
     */
    cb.reason = XmCR_HELP;
    cb.event = event;

    /* KAB: EVALUATE THIS CODE */
    do {
        if ((XtHasCallbacks(widget, XmNhelpCallback) == XtCallbackHasSome)) {
            XtCallCallbacks(widget, XmNhelpCallback, &cb);
            return;
        } else {
            widget = XtParent(widget);
        }
    }
    while (widget != NULL);

    widget = savedWidget;
    do {
	if (_XgSupplyGenericHelp(parent, widget)) {
            return;
        } else {
            widget = XtParent(widget);
        }
    }
    while (widget != NULL);

    /*
     * If all else fails, give up!!
     */
    XgWarningDialog(parent, "No help on this item!!!");

}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorHelp(w, client_data, call_data)
    Widget                          w;
    caddr_t                         client_data;
    caddr_t                         call_data;
#else
_XgInteractorHelp(
                  Widget w,
                  caddr_t client_data,
                  caddr_t call_data)
#endif
{
    XgHelpStruct                   *xgiHelp = (XgHelpStruct *) client_data;
    XmAnyCallbackStruct            *callback = (XmAnyCallbackStruct *) call_data;
    char                           *help_text = xgiHelp->text;
    Widget                          parent = w;
    Widget                          button = w;
    Widget                          widget;
    Widget                          savedWidget;

    if (*help_text == '$') {    /* Invoke context help. */
        Cursor                          cursor;
        XmAnyCallbackStruct             cb;

        /*
         * Create question mark cursor
         */
        cursor = XCreateFontCursor(XtDisplay(w), XC_question_arrow);

        /*
         * Get a widget selection from the user
         */
        savedWidget = widget = XmTrackingLocate(xgiHelp->widget, cursor, True);

        /*
         * Free the cursor resource
         */
        XFreeCursor(XtDisplay(w), cursor);

        /*
         * If (for some reason) we got a NULL widget, return.
         */
        if (widget == NULL)
            return;

        /*
         * Setup callback (call_data) information
         */
        cb.reason = XmCR_HELP;
        cb.event = (XEvent *) callback->event;

        /*
         * Traverse up the widget hierarchy looking for help callbacks or
         * generic help messages.
         */
	do {
	    if (XtHasCallbacks(widget, XmNhelpCallback) == XtCallbackHasSome) {
		XtCallCallbacks(widget, XmNhelpCallback, &cb);
		return;
	    } else {
		widget = XtParent(widget);
	    }
	}
	while (widget != NULL);

	widget = savedWidget;
	do {
	    if (_XgSupplyGenericHelp(xgiHelp->widget, widget)) {
		return;
	    } else {
		widget = XtParent(widget);
	    }
	}
        while (widget != NULL);

        /*
         * If all else fails, give up!!
         */
        XgWarningDialog(xgiHelp->widget, "No help on this item!!!");

    } else if (*help_text == '@') {     /* invoke help widget with
                                         * chapter/item */
	XmString xmlabel;

	XtVaGetValues(button, XmNlabelString, &xmlabel, NULL);
	XgDoHelpWidgetHelp(xgiHelp->widget, help_text + 1, xmlabel);
    } else {                    /* invoke info. dialog with string passed */
        if (xgiHelp->text)
            XgHelpCallback(xgiHelp->widget, (XtPointer) xgiHelp->text, NULL);
    }
}


/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
XgInteractorCallBack(w, client_data, call_data)
    Widget                          w;
    caddr_t                         client_data;
    caddr_t                         call_data;
#else
XgInteractorCallBack(
                     Widget w,
                     caddr_t client_data,
                     caddr_t call_data)
#endif
{
    unsigned char                   which_button = (unsigned char) client_data;
    InteractorWidget                xgi = (InteractorWidget) XtParent(w);
    XmAnyCallbackStruct            *callback = (XmAnyCallbackStruct *) call_data;
    InteractorCallbackStruct        temp;
    Boolean                         match = True;
    String                          text;

    temp.event = callback->event;
    if ( I_DialogType(xgi) == XmINTERACT_LIST_TYPE ) {
	text = XmTextGetString(I_ListText(xgi));
	temp.value = XmStringCreateSimple(text);
	temp.length = XmStringLength (temp.value);
    } else if ( I_DialogType(xgi) == XmINTERACT_PROMPT_TYPE ) {
	text = XmTextGetString(I_Text(xgi));
	temp.value = XmStringCreateSimple(text);
	temp.length = XmStringLength (temp.value);
    } else if ( XtIsManaged(I_ListText(xgi)) && 
		 XtIsManaged(XtParent(I_List(xgi)))) {
	text = XmTextGetString(I_ListText(xgi));
	temp.value = XmStringCreateSimple(text);
	temp.length = XmStringLength (temp.value);
    } else if ( XtIsManaged(I_Text(xgi)) ) {
	text = XmTextGetString(I_Text(xgi));
	temp.value = XmStringCreateSimple(text);
	temp.length = XmStringLength (temp.value);
    } else {
	temp.value = NULL;
    }

    switch (which_button) {
    case XmINTERACT_OK_BUTTON:
        if ( I_DialogType(xgi) == XmINTERACT_LIST_TYPE ||
	      ((I_List(xgi) != NULL) && XtIsManaged(XtParent(I_List(xgi)))) ) {
	    if ( I_ListMustMatch(xgi) ) {
		match = XmListItemExists(I_List(xgi), temp.value);
	    }
	    if ( !match ) {
		temp.reason = XmCR_NO_MATCH;
		XtCallCallbackList((Widget) xgi, I_ListNoMatchCallback(xgi),
		     &temp);
	    } else {
		temp.reason = XmCR_OK;
		XtCallCallbackList((Widget) xgi, I_OkCallback(xgi), &temp);
	    }
        } else {
	    temp.reason = XmCR_OK;
	    XtCallCallbackList((Widget) xgi, I_OkCallback(xgi), &temp);
        }
        break;

    case XmINTERACT_APPLY_BUTTON:
        temp.reason = XmCR_APPLY;
        XtCallCallbackList((Widget) xgi, I_ApplyCallback(xgi), &temp);
        break;

    case XmINTERACT_CANCEL_BUTTON:
        temp.reason = XmCR_CANCEL;
        XtCallCallbackList((Widget) xgi, I_CancelCallback(xgi), &temp);
        break;
    }
    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
ListCallBack (w, client_data, call_data)
            Widget          w ;
            caddr_t         client_data ;
            caddr_t         call_data ;
#else
ListCallBack(
            Widget          w,
            caddr_t         client_data,
            caddr_t         call_data)
#endif
{
    XmListCallbackStruct    *callback = (XmListCallbackStruct *) call_data;
    InteractorWidget        xgi = (InteractorWidget) client_data ;
    char                    *text;

    I_ListSelectedItemPosition(xgi) = callback->item_position;
    if ( XmStringGetLtoR(callback->item, XmSTRING_DEFAULT_CHARSET, &text) ) {
        XmTextSetString(I_ListText(xgi), text);
	I_ListTextString(xgi) = XmStringCreateSimple(text);
        XmTextSetCursorPosition(I_ListText(xgi), strlen (text));
        XtFree(text) ;
    }
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
TextCallBack (w, client_data, call_data)
            Widget          w ;
            caddr_t         client_data ;
            caddr_t         call_data ;
#else
TextCallBack(
            Widget          w,
            caddr_t         client_data,
            caddr_t         call_data)
#endif
{
    XmAnyCallbackStruct    *callback = (XmAnyCallbackStruct *) call_data;
    InteractorWidget        xgi = (InteractorWidget) client_data ;
    char                    *text;
    XmString               xms;
    Boolean                  match = False;

    text = XmTextGetString(I_ListText(xgi));
    if ( text == NULL ) return;
    if ( I_ListTextString(xgi) != NULL ) XmStringFree(I_ListTextString(xgi));
    I_ListTextString(xgi) = XmStringCreateSimple(text);
    xms = XmStringCreateSimple(text);
    match = XmListItemExists(I_List(xgi), xms);
    if ( match ) {
        XmListSelectItem(I_List(xgi), xms, False);
    } else {
        XmTextSetString(I_ListText(xgi),"");
    }
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
PromptTextCallBack (w, client_data, call_data)
            Widget          w ;
            caddr_t         client_data ;
            caddr_t         call_data ;
#else
PromptTextCallBack(
            Widget          w,
            caddr_t         client_data,
            caddr_t         call_data)
#endif
{
    XmAnyCallbackStruct    *callback = (XmAnyCallbackStruct *) call_data;
    InteractorWidget        xgi = (InteractorWidget) client_data ;
    char                    *text;
    XmString               xms;
    Boolean                  match = False;

    text = XmTextGetString(I_Text(xgi));
    if ( text == NULL ) return;
    if ( I_TextString(xgi) != NULL ) XmStringFree(I_TextString(xgi));
    I_TextString(xgi) = XmStringCreateSimple(text);
}


/*
 * FUNCTION
 */
static                          Boolean
#ifdef _NO_PROTO
SetValues(current, request, new)
    InteractorWidget                current;
    InteractorWidget                request;
    InteractorWidget                new;
#else
SetValues(
          InteractorWidget current,
          InteractorWidget request,
          InteractorWidget new)
#endif
{
    Arg                             al[10];
    register int                    ac = 0;
    Dimension                       w = 0, h = 0;

    XmGeoMatrix                     geoSpec = NULL;
    String                          text_value;

    BB_InSetValues(new) = True;

    if (I_OkLabelString(new) != I_OkLabelString(current)) {
        _XgInteractorUpdateString(I_OkButton(new), I_OkLabelString(new),
                     I_StringDirection(new));
        I_OkLabelString(new) = NULL;
    }
    if (I_ApplyLabelString(new) != I_ApplyLabelString(current)) {
        _XgInteractorUpdateString(I_ApplyButton(new), I_ApplyLabelString(new),
                     I_StringDirection(new));
        I_ApplyLabelString(new) = NULL;
    }
    if (I_CancelLabelString(new) != I_CancelLabelString(current)) {
        _XgInteractorUpdateString(I_CancelButton(new), I_CancelLabelString(new),
                     I_StringDirection(new));
        I_CancelLabelString(new) = NULL;
    }
    if (I_HelpLabelString(new) != I_HelpLabelString(current)) {
        _XgInteractorUpdateString(I_HelpButton(new), I_HelpLabelString(new),
                     I_StringDirection(new));
        I_HelpLabelString(new) = NULL;
    }
    if (I_PromptLabelString(new) != I_PromptLabelString(current)) {
        _XgInteractorUpdateString(I_PromptLabel(new), I_PromptLabelString(new),
                     I_StringDirection(new));
        I_PromptLabelString(new) = NULL;
    }
    if (I_ListLabelString(new) != I_ListLabelString(current)) {
        _XgInteractorUpdateString(I_ListLabel(new), I_ListLabelString(new),
                     I_StringDirection(new));
        I_ListLabelString(new) = NULL;
    }
    if (I_ListTextLabelString(new) != I_ListTextLabelString(current)) {
        _XgInteractorUpdateString(I_ListTextLabel(new), I_ListTextLabelString(new),
                     I_StringDirection(new));
        I_ListTextLabelString(new) = NULL;
    }
    /*
     * Validate dialog type.
     */
    if (I_DialogType(new) != I_DialogType(current)) {
        _XmWarning(new, WARN_INTERACT_TYPE_CHANGE);
        I_DialogType(new) = I_DialogType(current);
    }
    ac = 0;
    if (I_ListItems(new)) {
        XtSetArg(al[ac], XmNitems, I_ListItems(new));
        ac++;
        I_ListItems(new) = NULL;
    }
    if (I_ListItemCount(new) != UNSPECIFIED) {
        XtSetArg(al[ac], XmNitemCount, I_ListItemCount(new));
        ac++;
        I_ListItemCount(new) = UNSPECIFIED;
    }
    if (I_ListVisibleItemCount(new) !=
            I_ListVisibleItemCount(current)) {
        XtSetArg(al[ac], XmNvisibleItemCount, I_ListVisibleItemCount(new));
        ac++;
    }
    if (ac) {
        if (I_List(new))
            XtSetValues(I_List(new), al, ac);
    }
    text_value = NULL;
    ac = 0;
    if (I_TextString(new) != I_TextString(current)) {
        if (!XmStringGetLtoR(I_TextString(new), XmSTRING_DEFAULT_CHARSET,
                             &text_value)) {
            text_value = NULL;
        }
        XtSetArg(al[ac], XmNvalue, text_value);
        ac++;
        I_TextString(new) = (XmString) UNSPECIFIED;
    }
    if (I_TextColumns(new) !=
            I_TextColumns(current)) {
        XtSetArg(al[ac], XmNcolumns,
                 I_TextColumns(new));
        ac++;
    }
    if (ac) {
        if (I_Text(new))
            XtSetValues(I_Text(new), al, ac);
    }
    if (text_value) {
        if (I_Text(new))
            XmTextSetCursorPosition(I_Text(new),
                                    strlen(text_value));
        XtFree(text_value);
    }

    text_value = NULL;
    ac = 0;
    if (I_ListTextString(new) != I_ListTextString(current)) {
        if (!XmStringGetLtoR(I_ListTextString(new), XmSTRING_DEFAULT_CHARSET,
                             &text_value)) {
            text_value = NULL;
        }
        XtSetArg(al[ac], XmNvalue, text_value);
        ac++;
        I_ListTextString(new) = (XmString) UNSPECIFIED;
    }
    if (I_ListTextColumns(new) !=
            I_ListTextColumns(current)) {
        XtSetArg(al[ac], XmNcolumns,
                 I_ListTextColumns(new));
        ac++;
    }
    if (ac) {
        if (I_ListText(new))
            XtSetValues(I_ListText(new), al, ac);
    }
    if (text_value) {
        if (I_ListText(new))
            XmTextSetCursorPosition(I_ListText(new),
                                    strlen(text_value));
        XtFree(text_value);
    }
    /*
     * The help items CANNOT be changed...
     */
    if ((I_HelpItemLabels(new) != I_HelpItemLabels(current)) ||
            (I_HelpItems(new) != I_HelpItems(current)) ||
            (I_HelpItemCount(new) != I_HelpItemCount(current))) {
        _XmWarning(new, WARN_HELP_ITEMS);
    }
    BB_InSetValues(new) = False;

    /*
     * If this is the instantiated class then do layout.
     */
    if (XtClass(new) == interactorWidgetClass) {
        _XmBulletinBoardSizeUpdate((XmBulletinBoardWidget) new);
    }
    return (Boolean) (FALSE);
}

static void
#ifdef _NO_PROTO
Destroy(xgi)
    InteractorWidget                xgi;
#else
Destroy(InteractorWidget xgi)
#endif                          /* _NO_PROTO */
{
    if (I_HelpItems(xgi))
        ClearItems(xgi);
    if (I_HelpItemLabels(xgi))
        ClearItemLabels(xgi);
    FreeHelpStruct(I_HelpStructList(xgi));
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetTextString(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetTextString(
                           InteractorWidget xgi,
                           XrmQuark resource,
                           XtArgVal * value)
#endif
{
    String                          data = NULL;
    XmString                        text_string;
    Arg                             al[1];

    if (I_Text(xgi)) {
        XtSetArg(al[0], XmNvalue, &data);
        XtGetValues(I_Text(xgi), al, 1);
        text_string = XmStringLtoRCreate(data, XmSTRING_DEFAULT_CHARSET);
        *value = (XtArgVal) text_string;
    } else {
        *value = NULL;
    }
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetListTextString(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetListTextString(
                           InteractorWidget xgi,
                           XrmQuark resource,
                           XtArgVal * value)
#endif
{
    String                          data = NULL;
    XmString                        text_string;
    Arg                             al[1];

    if (I_ListText(xgi)) {
        XtSetArg(al[0], XmNvalue, &data);
        XtGetValues(I_ListText(xgi), al, 1);
        text_string = XmStringLtoRCreate(data, XmSTRING_DEFAULT_CHARSET);
        *value = (XtArgVal) text_string;
    } else {
        *value = NULL;
    }
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetListItems(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetListItems(
                          InteractorWidget xgi,
                          XrmQuark resource,
                          XtArgVal * value)
#endif
{
    Arg                             al[1];
    XmString                        data;

    if (I_List(xgi)) {
        XtSetArg(al[0], XmNitems, &data);
        XtGetValues(I_List(xgi), al, 1);
        *value = (XtArgVal) data;
    } else {
        *value = NULL;
    }
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetListItemCount(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetListItemCount(
                              InteractorWidget xgi,
                              XrmQuark resource,
                              XtArgVal * value)
#endif
{
    int                             data;
    Arg                             al[1];

    if (I_List(xgi)) {
        XtSetArg(al[0], XmNitemCount, &data);
        XtGetValues(I_List(xgi), al, 1);
        *value = (XtArgVal) data;
    } else {
        *value = 0;
    }
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetListVisibleItemCount(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetListVisibleItemCount(
                                     InteractorWidget xgi,
                                     XrmQuark resource,
                                     XtArgVal * value)
#endif
{
    int                             data;
    Arg                             al[1];

    if (I_List(xgi)) {
        XtSetArg(al[0], XmNvisibleItemCount, &data);
        XtGetValues(I_List(xgi), al, 1);
        *value = (XtArgVal) data;
    } else {
        *value = 0;
    }
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetTextColumns(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetTextColumns(
                            InteractorWidget xgi,
                            XrmQuark resource,
                            XtArgVal * value)
#endif
{
    short                           data;
    Arg                             al[1];

    if (I_Text(xgi)) {
        XtSetArg(al[0], XmNcolumns, &data);
        XtGetValues(I_Text(xgi), al, 1);
        *value = (XtArgVal) data;
    } else {
        *value = 0;
    }
    return;
}


/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetListTextColumns(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetListTextColumns(
                            InteractorWidget xgi,
                            XrmQuark resource,
                            XtArgVal * value)
#endif
{
    short                           data;
    Arg                             al[1];

    if (I_ListText(xgi)) {
        XtSetArg(al[0], XmNcolumns, &data);
        XtGetValues(I_ListText(xgi), al, 1);
        *value = (XtArgVal) data;
    } else {
        *value = 0;
    }
    return;
}


/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetPromptLabelString(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetPromptLabelString(
                                  InteractorWidget xgi,
                                  XrmQuark resource,
                                  XtArgVal * value)
#endif
{
    XmString                        data;
    Arg                             al[1];

    if (I_PromptLabel(xgi)) {
        XtSetArg(al[0], XmNlabelString, &data);
        XtGetValues(I_PromptLabel(xgi), al, 1);
        *value = (XtArgVal) data;
    } else {
        *value = NULL;
    }
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetListLabelString(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetListLabelString(
                                InteractorWidget xgi,
                                XrmQuark resource,
                                XtArgVal * value)
#endif
{
    XmString                        data;
    Arg                             al[1];

    if (I_ListLabel(xgi)) {
        XtSetArg(al[0], XmNlabelString, &data);
        XtGetValues(I_ListLabel(xgi), al, 1);
        *value = (XtArgVal) data;
    } else {
        *value = NULL;
    }
    return;
}


/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetListTextLabelString(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetListTextLabelString(
                                InteractorWidget xgi,
                                XrmQuark resource,
                                XtArgVal * value)
#endif
{
    XmString                        data;
    Arg                             al[1];

    if (I_ListTextLabel(xgi)) {
        XtSetArg(al[0], XmNlabelString, &data);
        XtGetValues(I_ListTextLabel(xgi), al, 1);
        *value = (XtArgVal) data;
    } else {
        *value = NULL;
    }
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetOkLabelString(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetOkLabelString(
                              InteractorWidget xgi,
                              XrmQuark resource,
                              XtArgVal * value)
#endif
{
    XmString                        data;
    Arg                             al[1];

    if (I_OkButton(xgi)) {
        XtSetArg(al[0], XmNlabelString, &data);
        XtGetValues(I_OkButton(xgi), al, 1);
        *value = (XtArgVal) data;
    } else {
        *value = NULL;
    }
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetApplyLabelString(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetApplyLabelString(
                                 InteractorWidget xgi,
                                 XrmQuark resource,
                                 XtArgVal * value)
#endif
{
    XmString                        data;
    Arg                             al[1];

    if (I_ApplyButton(xgi)) {
        XtSetArg(al[0], XmNlabelString, &data);
        XtGetValues(I_ApplyButton(xgi), al, 1);
        *value = (XtArgVal) data;
    } else {
        *value = NULL;
    }
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetCancelLabelString(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetCancelLabelString(
                                  InteractorWidget xgi,
                                  XrmQuark resource,
                                  XtArgVal * value)
#endif
{
    XmString                        data;
    Arg                             al[1];

    if (I_CancelButton(xgi)) {
        XtSetArg(al[0], XmNlabelString, &data);
        XtGetValues(I_CancelButton(xgi), al, 1);
        *value = (XtArgVal) data;
    } else {
        *value = NULL;
    }
    return;
}

/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorGetHelpLabelString(xgi, resource, value)
    InteractorWidget                xgi;
    XrmQuark                        resource;
    XtArgVal                       *value;
#else
_XgInteractorGetHelpLabelString(
                                InteractorWidget xgi,
                                XrmQuark resource,
                                XtArgVal * value)
#endif
{
    XmString                        data;
    Arg                             al[1];

    if (I_HelpButton(xgi)) {
        XtSetArg(al[0], XmNlabelString, &data);
        XtGetValues(I_HelpButton(xgi), al, 1);
        *value = (XtArgVal) data;
    } else {
        *value = NULL;
    }
    return;
}


/*
 * FUNCTION
 */
void
#ifdef _NO_PROTO
_XgInteractorUpdateString(w, string, direction)
    Widget                          w;
    XmString                        string;
    XmStringDirection               direction;
#else
_XgInteractorUpdateString(
             Widget w,
             XmString string,
             XmStringDirection direction)
#endif
{
    Arg                             al[3];
    register int                    ac = 0;

    if (w) {
        XtSetArg(al[ac], XmNstringDirection, direction);
        ac++;
        XtSetArg(al[ac], XmNlabelString, string);
        ac++;
        XtSetValues(w, al, ac);
    }
    return;
}

/*
 * CopyItems - Copy the item list into our space.
 * 
 */
static void
CopyItems(xgi)
    InteractorWidget                xgi;
{
    register int                    i;
    char                          **il;

    if (I_HelpItems(xgi) && I_HelpItemCount(xgi)) {
        il = (char **) XtMalloc(sizeof(char *) * (I_HelpItemCount(xgi)));
        for (i = 0; i < I_HelpItemCount(xgi); i++)
            il[i] = XtNewString(I_HelpItems(xgi)[i]);

        I_HelpItems(xgi) = il;
        for (i = 0; i < I_HelpItemCount(xgi); i++)
            I_HelpItems(xgi)[i] = il[i];
    }
}



/*
 * CopyItemLabels - Copy the item label list into our space.
 * 
 */
static void
CopyItemLabels(xgi)
    InteractorWidget                xgi;
{
    register int                    i;
    XmString                       *il;

    if (I_HelpItemLabels(xgi) && I_HelpItemCount(xgi)) {
        il = (XmString *) XtMalloc(sizeof(XmString) * (I_HelpItemCount(xgi)));
        for (i = 0; i < I_HelpItemCount(xgi); i++)
            il[i] = XmStringCopy(I_HelpItemLabels(xgi)[i]);

        I_HelpItemLabels(xgi) = il;
        for (i = 0; i < I_HelpItemCount(xgi); i++)
            I_HelpItemLabels(xgi)[i] = il[i];
    }
}

static void
#ifdef _NO_PROTO
FreeHelpStruct(ptr)
    XgHelpStruct * ptr;
#else
FreeHelpStruct( XgHelpStruct * ptr)
#endif
{
    if ( ptr == NULL ) return;
    FreeHelpStruct(ptr->next);
    XtFree(ptr);
}

static XgHelpStruct *
#ifdef _NO_PROTO
AllocHelpStruct(xgi)
    InteractorWidget                xgi;
#else
AllocHelpStruct( InteractorWidget xgi)
#endif
{
    XgHelpStruct * xgihs;

    if ( I_HelpStructList(xgi) == NULL ) {
	I_HelpStructList(xgi) = 
	   xgihs = (XgHelpStruct *) XtMalloc(sizeof(XgHelpStruct));
	bzero((char *)xgihs, sizeof(XgHelpStruct));
    } else {
	XgHelpStruct *ptr = I_HelpStructList(xgi);

        while ( ptr->next ) {
	    ptr = ptr->next;
	}
	ptr->next = xgihs = (XgHelpStruct *) XtMalloc(sizeof(XgHelpStruct));
	bzero((char *)xgihs, sizeof(XgHelpStruct));
    }
    return xgihs;
}

/*
 * ClearItems - delete all elements from the item list, and free the space
 * associated with it.
 * 
 */
static void
ClearItems(xgi)
    InteractorWidget                xgi;
{
    register int                    i;

    if (!(I_HelpItems(xgi) && I_HelpItemCount(xgi)))
        return;
    for (i = 0; i < I_HelpItemCount(xgi); i++)
        XtFree(I_HelpItems(xgi)[i]);
    XtFree(I_HelpItems(xgi));
    I_HelpItems(xgi) = NULL;
}

/*
 * ClearItemLabels - delete all elements from the item label list, and free
 * the space associated with it.
 * 
 */
static void
ClearItemLabels(xgi)
    InteractorWidget                xgi;
{
    register int                    i;

    if (!(I_HelpItemLabels(xgi) && I_HelpItemCount(xgi)))
        return;
    for (i = 0; i < I_HelpItemCount(xgi); i++)
        XmStringFree(I_HelpItemLabels(xgi)[i]);
    XtFree(I_HelpItemLabels(xgi));
    I_HelpItemLabels(xgi) = NULL;
}


/*
 * PUBLIC API
 */

/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgInteractorGetChild(xgi, which)
    Widget                          xgi;        /* Interactor widget   */
    unsigned char                   which;      /* which child           */
#else
XgInteractorGetChild(
                     Widget xgi,/* Interactor widget   */
#if NeedWidePrototypes
                     unsigned int which
#else
                     unsigned char which
#endif
)
#endif
{
    Widget                          child = NULL;

    switch (which) {
    case XmINTERACT_WORK_AREA:
        child = I_WorkArea(xgi);
        break;

    case XmINTERACT_PROMPT_LABEL:
        child = I_PromptLabel(xgi);
        break;

    case XmINTERACT_TEXT:
        child = I_Text(xgi);
        break;

    case XmINTERACT_LIST_LABEL:
        child = I_ListLabel(xgi);
        break;

    case XmINTERACT_LIST:
        child = I_List(xgi);
        break;

    case XmINTERACT_LIST_TEXT_LABEL:
        child = I_ListTextLabel(xgi);
        break;

    case XmINTERACT_LIST_TEXT:
        child = I_ListText(xgi);
        break;

    case XmINTERACT_SEPARATOR:
        child = I_Separator(xgi);
        break;

    case XmINTERACT_OK_BUTTON:
        child = I_OkButton(xgi);
        break;

    case XmINTERACT_APPLY_BUTTON:
        child = I_ApplyButton(xgi);
        break;

    case XmINTERACT_CANCEL_BUTTON:
        child = I_CancelButton(xgi);
        break;

    case XmINTERACT_HELP_BUTTON:
        child = I_HelpButton(xgi);
        break;

    default:
        _XmWarning(xgi, WARN_CHILD_TYPE);
        break;
    }
    return (child);
}


/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgCreateInteractor(p, name, args, n)
    Widget                          p;  /* parent widget         */
    String                          name;       /* widget name           */
    ArgList                         args;       /* arg list      */
    Cardinal                        n;  /* arg count     */
#else
XgCreateInteractor(
                   Widget p,    /* parent widget         */
                   String name, /* widget name           */
                   ArgList args,/* arg list      */
                   Cardinal n)  /* arg count     */
#endif
{
    return (XtCreateWidget(name, interactorWidgetClass, p, args, n));
}

/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgCreateInteractorTopLevel(ds_p, name, xgi_args, xgi_n)
    Widget                          ds_p;       /* parent for shell      */
    String                          name;       /* widget name           */
    ArgList                         xgi_args;   /* arglist for xgi       */
    Cardinal                        xgi_n;      /* argcount for xgi      */
#else
XgCreateInteractorTopLevel(
                         Widget ds_p,   /* parent for shell      */
                         String name,   /* widget name           */
                         ArgList xgi_args,      /* arglist for xgi       */
                         Cardinal xgi_n)        /* argcount for xgi      */
#endif
{
    Widget                          ds; /* DialogShell           */
    Arg                             ds_args[10];        /* arglist for shell     */
    ArgList                         _xgi_args;  /* arglist for xgi       */
    Widget                          xgi;        /* new xgi widget        */
    char                           *ds_name;
    int ac;

    /*
     * create DialogShell parent
     */
    ds_name = XtCalloc(strlen(name) + 1, sizeof(char));
    strcpy(ds_name, name);

    ac = 0;
    XtSetArg(ds_args[ac], XmNallowShellResize, True); ac++;
    XtSetArg(ds_args[ac], XmNwidth, 100); ac++;
    XtSetArg(ds_args[ac], XmNheight, 100); ac++;
    ds = XtCreateWidget(ds_name, topLevelShellWidgetClass, ds_p, ds_args, ac);

    XtFree(ds_name);

    /*
     * allocate arglist, copy args, add dialog type arg
     */
    _xgi_args = (ArgList) XtMalloc(sizeof(Arg) * (xgi_n + 1));

    bcopy(xgi_args, _xgi_args, sizeof(Arg) * xgi_n);
    XtSetArg(_xgi_args[xgi_n], XmNdialogType, XmINTERACT_DIALOG_TYPE);
    xgi_n++;

    /*
     * create Interactor, free args, return
     */
    xgi = XtCreateWidget(name, interactorWidgetClass,
                         ds, _xgi_args, xgi_n);
    XtAddCallback(xgi, XmNdestroyCallback, _XmDestroyParentCallback, NULL);

    XtFree(_xgi_args);
    return (xgi);
}


/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgCreateInteractorDialog(ds_p, name, xgi_args, xgi_n)
    Widget                          ds_p;       /* parent for shell      */
    String                          name;       /* widget name           */
    ArgList                         xgi_args;   /* arglist for xgi       */
    Cardinal                        xgi_n;      /* argcount for xgi      */
#else
XgCreateInteractorDialog(
                         Widget ds_p,   /* parent for shell      */
                         String name,   /* widget name           */
                         ArgList xgi_args,      /* arglist for xgi       */
                         Cardinal xgi_n)        /* argcount for xgi      */
#endif
{
    Widget                          ds; /* DialogShell           */
    Arg                             ds_args[10];        /* arglist for shell     */
    ArgList                         _xgi_args;  /* arglist for xgi       */
    Widget                          xgi;        /* new xgi widget        */
    char                           *ds_name;

    /*
     * create DialogShell parent
     */
    ds_name = XtCalloc(strlen(name) + 1, sizeof(char));
    strcpy(ds_name, name);

    XtSetArg(ds_args[0], XmNallowShellResize, True);
    ds = XmCreateDialogShell(ds_p, ds_name, ds_args, 1);

    XtFree(ds_name);

    /*
     * allocate arglist, copy args, add dialog type arg
     */
    _xgi_args = (ArgList) XtMalloc(sizeof(Arg) * (xgi_n + 1));

    bcopy(xgi_args, _xgi_args, sizeof(Arg) * xgi_n);
    XtSetArg(_xgi_args[xgi_n], XmNdialogType, XmINTERACT_DIALOG_TYPE);
    xgi_n++;

    /*
     * create Interactor, free args, return
     */
    xgi = XtCreateWidget(name, interactorWidgetClass,
                         ds, _xgi_args, xgi_n);
    XtAddCallback(xgi, XmNdestroyCallback, _XmDestroyParentCallback, NULL);

    XtFree(_xgi_args);
    return (xgi);
}


/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgCreateInteractorPrompt(p, name, xgi_args, xgi_n)
    Widget                          p;  /* parent widget         */
    String                          name;       /* widget name           */
    ArgList                         xgi_args;       /* arg list      */
    Cardinal                        xgi_n;  /* arg count     */
#else
XgCreateInteractorPrompt(
                         Widget p,      /* parent widget         */
                         String name,   /* widget name           */
                         ArgList xgi_args,  /* arg list      */
                         Cardinal xgi_n)    /* arg count     */
#endif
{
    Widget                          widget;
    ArgList                         _xgi_args;  /* arglist for xgi       */

    /*
     * allocate arglist, copy args, add dialog type arg
     */
    _xgi_args = (ArgList) XtMalloc(sizeof(Arg) * (xgi_n + 1));

    bcopy(xgi_args, _xgi_args, sizeof(Arg) * xgi_n);
    XtSetArg(_xgi_args[xgi_n], XmNdialogType, XmINTERACT_PROMPT_TYPE);
    xgi_n++;
    widget = XtCreateWidget(name, interactorWidgetClass, p, _xgi_args, xgi_n);

    return widget;
}

/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgCreateInteractorPromptDialog(ds_p, name, xgi_args, xgi_n)
    Widget                          ds_p;       /* parent for shell      */
    String                          name;       /* widget name           */
    ArgList                         xgi_args;   /* arglist for xgi       */
    Cardinal                        xgi_n;      /* argcount for xgi      */
#else
XgCreateInteractorPromptDialog(
                               Widget ds_p,     /* parent for shell      */
                               String name,     /* widget name           */
                               ArgList xgi_args,        /* arglist for xgi       */
                               Cardinal xgi_n)  /* argcount for xgi      */
#endif
{
    Widget                          ds; /* DialogShell           */
    Arg                             ds_args[10];        /* arglist for shell     */
    ArgList                         _xgi_args;  /* arglist for xgi       */
    Widget                          xgi;        /* new xgi widget        */
    char                           *ds_name;

    /*
     * create DialogShell parent
     */
    ds_name = XtCalloc(strlen(name) + 1, sizeof(char));
    strcpy(ds_name, name);

    XtSetArg(ds_args[0], XmNallowShellResize, True);
    ds = XmCreateDialogShell(ds_p, ds_name, ds_args, 1);

    XtFree(ds_name);

    /*
     * allocate arglist, copy args, add dialog type arg
     */
    _xgi_args = (ArgList) XtMalloc(sizeof(Arg) * (xgi_n + 1));

    bcopy(xgi_args, _xgi_args, sizeof(Arg) * xgi_n);
    XtSetArg(_xgi_args[xgi_n], XmNdialogType, XmINTERACT_PROMPT_TYPE);
    xgi_n++;


    /*
     * create Interactor, free args, return
     */
    xgi = XtCreateWidget(name, interactorWidgetClass,
                         ds, _xgi_args, xgi_n);
    XtAddCallback(xgi, XmNdestroyCallback, _XmDestroyParentCallback, NULL);

    XtFree(_xgi_args);
    return (xgi);
}

/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgCreateInteractorList(p, name, xgi_args, xgi_n)
    Widget                          p;  /* parent widget         */
    String                          name;       /* widget name           */
    ArgList                         xgi_args;       /* arg list      */
    Cardinal                        xgi_n;  /* arg count     */
#else
XgCreateInteractorList(
                       Widget p,/* parent widget         */
                       String name,     /* widget name           */
                       ArgList xgi_args,    /* arg list      */
                       Cardinal xgi_n)      /* arg count     */
#endif
{
    Widget                          widget;
    ArgList                         _xgi_args;  /* arglist for xgi       */

    /*
     * allocate arglist, copy args, add dialog type arg
     */
    _xgi_args = (ArgList) XtMalloc(sizeof(Arg) * (xgi_n + 1));

    bcopy(xgi_args, _xgi_args, sizeof(Arg) * xgi_n);
    XtSetArg(_xgi_args[xgi_n], XmNdialogType, XmINTERACT_LIST_TYPE);
    xgi_n++;
    widget = XtCreateWidget(name, interactorWidgetClass, p, _xgi_args, xgi_n);

    return widget;
}

/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgCreateInteractorListDialog(ds_p, name, xgi_args, xgi_n)
    Widget                          ds_p;       /* parent for shell      */
    String                          name;       /* widget name           */
    ArgList                         xgi_args;   /* arglist for xgi       */
    Cardinal                        xgi_n;      /* argcount for xgi      */
#else
XgCreateInteractorListDialog(
                             Widget ds_p,       /* parent for shell      */
                             String name,       /* widget name           */
                             ArgList xgi_args,  /* arglist for xgi       */
                             Cardinal xgi_n)    /* argcount for xgi      */
#endif
{
    Widget                          ds; /* DialogShell           */
    Arg                             ds_args[10];        /* arglist for shell     */
    ArgList                         _xgi_args;  /* arglist for xgi       */
    Widget                          xgi;        /* new xgi widget        */
    char                           *ds_name;

    /*
     * create DialogShell parent
     */
    ds_name = XtCalloc(strlen(name) + 1, sizeof(char));
    strcpy(ds_name, name);

    XtSetArg(ds_args[0], XmNallowShellResize, True);
    ds = XmCreateDialogShell(ds_p, ds_name, ds_args, 1);

    XtFree(ds_name);

    /*
     * allocate arglist, copy args, add dialog type arg
     */
    _xgi_args = (ArgList) XtMalloc(sizeof(Arg) * (xgi_n + 1));

    bcopy(xgi_args, _xgi_args, sizeof(Arg) * xgi_n);
    XtSetArg(_xgi_args[xgi_n], XmNdialogType, XmINTERACT_LIST_TYPE);
    xgi_n++;


    /*
     * create Interactor, free args, return
     */
    xgi = XtCreateWidget(name, interactorWidgetClass,
                         ds, _xgi_args, xgi_n);
    XtAddCallback(xgi, XmNdestroyCallback, _XmDestroyParentCallback, NULL);

    XtFree(_xgi_args);
    return (xgi);
}

static void
#ifdef _NO_PROTO
XgDoHelpWidgetHelp(w, s, title)
Widget w;
char *s;
XmString title;
#else
XgDoHelpWidgetHelp(Widget w, char *s, XmString title)
#endif
{
    Widget xgh;
    XmString xms1;
    char *helpDir;
    char buf[1024];
    Arg al[5];
    int ac = 0;

    if ( (helpDir = (char *)getenv("XGRASSHELPDIR")) == NULL ) {
	XgWarningDialog(w, "XGRASSHELPDIR not set....");
	return;
    }

    XtSetArg(al[ac], XmNdialogTitle, title); ac++;
    XtSetArg(al[ac], XmNhelpFile, s); ac++;
    XtSetArg(al[ac], XmNdismissOnly, True); ac++;
    xgh = XgCreateHelpDialog(w, "help_widget", al, ac);
    XtManageChild(xgh);
}

/*
 ***************************************************************************
 * Convinience function that creates an InteractorListDialog and sticks a
 * list of fonts in it.
 ***************************************************************************
 */
Widget
#ifdef _NO_PROTO
XgCreateInteractorFontListDialog(ds_p, name, xgi_args, xgi_n)
    Widget                          ds_p;       /* parent for shell      */
    String                          name;       /* widget name           */
    ArgList                         xgi_args;   /* arglist for xgi       */
    Cardinal                        xgi_n;      /* argcount for xgi      */
#else
XgCreateInteractorFontListDialog(
                             Widget ds_p,       /* parent for shell      */
                             String name,       /* widget name           */
                             ArgList xgi_args,  /* arglist for xgi       */
                             Cardinal xgi_n)    /* argcount for xgi      */
#endif
{
    char     **fonts;
    int      numfonts = 0;
    int      i;
    XmString xs;
    Widget   list, listdg;
    
    /* Create the Interactor Dialog */
    listdg = XgCreateInteractorListDialog(ds_p, name, xgi_args, xgi_n);

    list = XgInteractorGetChild(listdg, XmINTERACT_LIST);
    fonts = XListFonts(XtDisplay(ds_p), "*", 10000, &numfonts);
    for (i = 0; i < numfonts; i++){
      xs = XmStringCreateSimple(fonts[i]);
      XmListAddItem(list, xs, (i==0?1:0));
      XmStringFree(xs);
    }
    

    return (listdg);
}

