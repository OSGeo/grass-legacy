#ifndef _InteractorP_h
#define _InteractorP_h

#include <Xm/BulletinBP.h>
#include <Interact.h>

/* Defines for use in allocation geometry matrix. */

#ifndef XmINTERACTOR_BIT
#define XmINTERACTOR_BIT  (49)
#endif

#define I_MAX_WIDGETS_VERT	7
#define I_MAX_NUM_WIDGETS	12

#define UNSPECIFIED		(~0L)

/*  New fields for the Interactor widget class record  */

typedef struct
{
	caddr_t		extension;      /* Pointer to extension record */
} InteractorClassPart;


/* Full class record declaration */

typedef struct _InteractorClassRec
{
	CoreClassPart			core_class;
	CompositeClassPart		composite_class;
	ConstraintClassPart		constraint_class;
	XmManagerClassPart		manager_class;
	XmBulletinBoardClassPart	bulletin_board_class;
	InteractorClassPart		interactor_class;
} InteractorClassRec;

externalref InteractorClassRec interactorClassRec;


/* New fields for the Interactor widget record */

typedef struct
{
	Widget		prompt;			/*  prompt widget  */
	XmString	prompt_label_string;	

	Widget		text;			/*  text widget  */
	XmString        text_string;
	short           text_columns;

	Widget          list_text;              /* list assoc'd text */
	XmString        list_text_string;
	short           list_text_columns;
	Widget          list_text_label;
	XmString        list_text_label_string;

	Widget		list_label;		/*  list label widget  */
	XmString	list_label_string;	

	Widget		list;			/*  list widget  */
	XmStringTable   list_items;
	int             list_item_count;
	int             list_visible_item_count;
	int             list_selected_item_pos;

	Boolean         must_match;
	XtCallbackList	no_match_callback;

	Widget		work_area;		/*  other widget  */

	Widget		separator;		/*  separator  */

	Widget		ok_button;		/*  enter button  */
	XmString	ok_label_string;

	Widget		apply_button;		/*  apply button  */
	XmString	apply_label_string;

	Widget		cancel_button;		/*  cancel button  */
	XmString	cancel_label_string;	/*  cancel button label  */

	Widget		help_button;		/*  help button  */
	Widget		help_cascade_button;	/*  help cascade button  */
	XmString	help_label_string;

	Widget          help_menu;              /* help popup */
	XmStringTable   help_item_labels;
	String *        help_items;
	int             help_item_count;
	XgHelpStruct   *help_struct_list;

	XtCallbackList	ok_callback;		/*  callbacks  */
	XtCallbackList	apply_callback;
	XtCallbackList	cancel_callback;

	Boolean		adding_xgi_widgets;
	Boolean		minimize_buttons;
	Boolean		work_area_stretch;

	unsigned char	dialog_type;		/*  prompt or selection  */
	Cursor help_cursor;                     /* help cursor... */
} InteractorPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _InteractorRec
{
	CorePart		core;
	CompositePart		composite;
	ConstraintPart		constraint;
	XmManagerPart		manager;
	XmBulletinBoardPart	bulletin_board;
	InteractorPart	interactor;
} InteractorRec;


#ifndef XgIsInteractor
#define XgIsInteractor(w)  (XtIsSubclass (w, interactorWidgetClass))
#endif


/*  access macros  */

#define I_DialogType(w) (((InteractorWidget) \
    (w))->interactor.dialog_type)
#define I_PromptLabel(w) (((InteractorWidget) \
    (w))->interactor.prompt)
#define I_PromptLabelString(w) (((InteractorWidget) \
    (w))->interactor.prompt_label_string)
#define I_Text(w) (((InteractorWidget) \
    (w))->interactor.text)
#define I_TextColumns(w) (((InteractorWidget) \
    (w))->interactor.text_columns)
#define I_TextString(w) (((InteractorWidget) \
    (w))->interactor.text_string)
#define I_ListText(w) (((InteractorWidget) \
    (w))->interactor.list_text)
#define I_ListTextLabel(w) (((InteractorWidget) \
    (w))->interactor.list_text_label)
#define I_ListTextLabelString(w) (((InteractorWidget) \
    (w))->interactor.list_text_label_string)
#define I_ListTextColumns(w) (((InteractorWidget) \
    (w))->interactor.list_text_columns)
#define I_ListTextString(w) (((InteractorWidget) \
    (w))->interactor.list_text_string)
#define I_ListLabelString(w) (((InteractorWidget) \
    (w))->interactor.list_label_string)
#define I_ListLabel(w) (((InteractorWidget) \
    (w))->interactor.list_label)
#define I_List(w) (((InteractorWidget) \
    (w))->interactor.list)
#define I_ListItems(w) (((InteractorWidget) \
    (w))->interactor.list_items)
#define I_ListItemCount(w) (((InteractorWidget) \
    (w))->interactor.list_item_count)
#define I_ListVisibleItemCount(w) (((InteractorWidget) \
    (w))->interactor.list_visible_item_count)
#define I_ListSelectedItemPosition(w) (((InteractorWidget) \
    (w))->interactor.list_selected_item_pos)
#define I_ListMustMatch(w) (((InteractorWidget) \
    (w))->interactor.must_match)
#define I_ListNoMatchCallback(w) (((InteractorWidget) \
    (w))->interactor.no_match_callback)
#define I_WorkArea(w) (((InteractorWidget) \
    (w))->interactor.work_area)
#define I_Separator(w) (((InteractorWidget) \
    (w))->interactor.separator)
#define I_OkButton(w) (((InteractorWidget) \
    (w))->interactor.ok_button)
#define I_OkLabelString(w) (((InteractorWidget) \
    (w))->interactor.ok_label_string)
#define I_OkCallback(w) (((InteractorWidget) \
    (w))->interactor.ok_callback)
#define I_ApplyButton(w) (((InteractorWidget) \
    (w))->interactor.apply_button)
#define I_ApplyLabelString(w) (((InteractorWidget) \
    (w))->interactor.apply_label_string)
#define I_ApplyCallback(w) (((InteractorWidget) \
    (w))->interactor.apply_callback)
#define I_CancelButton(w) (((InteractorWidget) \
    (w))->bulletin_board.cancel_button)
#define I_CancelLabelString(w) (((InteractorWidget) \
    (w))->interactor.cancel_label_string)
#define I_CancelCallback(w) (((InteractorWidget) \
    (w))->interactor.cancel_callback)
#define I_HelpCascadeButton(w) (((InteractorWidget) \
    (w))->interactor.help_cascade_button)
#define I_HelpButton(w) (((InteractorWidget) \
    (w))->interactor.help_button)
#define I_HelpLabelString(w) (((InteractorWidget) \
    (w))->interactor.help_label_string)
#define I_HelpMenu(w) (((InteractorWidget) \
    (w))->interactor.help_menu)
#define I_HelpItemLabels(w) (((InteractorWidget) \
    (w))->interactor.help_item_labels)
#define I_HelpItems(w) (((InteractorWidget) \
    (w))->interactor.help_items)
#define I_HelpItemCount(w) (((InteractorWidget) \
    (w))->interactor.help_item_count)
#define I_HelpStructList(w) (((InteractorWidget) \
    (w))->interactor.help_struct_list)
#define I_HelpCursor(w) (((InteractorWidget) \
    (w))->interactor.help_cursor)
#define I_DefaultButton(w) (((InteractorWidget) \
    (w))->bulletin_board.default_button)
#define I_MarginHeight(w) (((InteractorWidget) \
    (w))->bulletin_board.margin_height)
#define I_MarginWidth(w) (((InteractorWidget) \
    (w))->bulletin_board.margin_width)
#define I_ButtonFontList(w) (((InteractorWidget) \
    (w))->bulletin_board.button_font_list)
#define I_LabelFontList(w) (((InteractorWidget) \
    (w))->bulletin_board.label_font_list)
#define I_StringDirection( w) (((InteractorWidget) \
    (w))->manager.string_direction)
#define I_AddingIWidgets( w) (((InteractorWidget) \
    (w))->interactor.adding_xgi_widgets)
#define I_EnableWorkAreaStretch( w) (((InteractorWidget) \
    (w))->interactor.work_area_stretch)
#define I_MinimizeButtons( w) (((InteractorWidget) \
    (w))->interactor.minimize_buttons)

/* Private external functions:
*/
#ifdef _NO_PROTO

extern void _XgInteractorHelp() ;
extern void _XgInteractorCreatePromptLabel() ;
extern void _XgInteractorCreateText() ;
extern void _XgInteractorCreateListLabel() ;
extern void _XgInteractorCreateList() ;
extern void _XgInteractorCreateListTextLabel() ;
extern void _XgInteractorCreateListText() ;
extern void _XgInteractorCreateSeparator() ;
extern void _XgInteractorCreateOkButton() ;
extern void _XgInteractorCreateApplyButton() ;
extern void _XgInteractorCreateCancelButton() ;
extern void _XgInteractorCreateHelpButton() ;
extern XmGeoMatrix _XgInteractorGeoMatrixCreate() ;
extern Boolean _XgInteractorNoGeoRequest() ;
extern void _XgInteractorGetTextColumns();
extern void _XgInteractorGetTextString();
extern void _XgInteractorGetListTextColumns();
extern void _XgInteractorGetListTextString();
extern void _XgInteractorGetListItems();
extern void _XgInteractorGetListItemCount();
extern void _XgInteractorGetListVisibleItemCount();
extern void _XgInteractorGetPromptLabelString() ;
extern void _XgInteractorGetListLabelString() ;
extern void _XgInteractorGetListTextLabelString() ;
extern void _XgInteractorGetOkLabelString() ;
extern void _XgInteractorGetApplyLabelString() ;
extern void _XgInteractorGetCancelLabelString() ;
extern void _XgInteractorGetHelpLabelString() ;
extern void _XgCopyItems();
extern void _XgCopyItemLabels();
extern void _XgClearItems();
extern void _XgClearItemLabels();

#else /* _NO_PROTO */

extern void _XgInteractorHelp(Widget w, caddr_t client_data, caddr_t call_data) ;
extern void _XgInteractorCreatePromptLabel( InteractorWidget xgi) ;
extern void _XgInteractorCreateText( InteractorWidget xgi) ;
extern void _XgInteractorCreateListLabel( InteractorWidget xgi) ;
extern void _XgInteractorCreateList( InteractorWidget xgi) ;
extern void _XgInteractorCreateListText( InteractorWidget xgi) ;
extern void _XgInteractorCreateListTextLabel( InteractorWidget xgi) ;
extern void _XgInteractorCreateSeparator( InteractorWidget xgi) ;
extern void _XgInteractorCreateOkButton( InteractorWidget xgi) ;
extern void _XgInteractorCreateApplyButton( InteractorWidget xgi) ;
extern void _XgInteractorCreateCancelButton( InteractorWidget xgi) ;
extern void _XgInteractorCreateHelpButton( InteractorWidget xgi) ;
extern XmGeoMatrix _XgInteractorGeoMatrixCreate( InteractorWidget sb,
                                Widget instigator, XtWidgetGeometry *desired) ;
extern Boolean _XgInteractorNoGeoRequest( XmGeoMatrix geoSpec) ;
extern void _XgInteractorGetTextColumns( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetTextString( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetListTextColumns( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetListTextString( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetListItems( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetListItemCount( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetListVisibleItemCount( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetPromptLabelString( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetListLabelString( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetListTextLabelString( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetOkLabelString( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetApplyLabelString( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetCancelLabelString( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
extern void _XgInteractorGetHelpLabelString( InteractorWidget xgi,
                                          XrmQuark resource, XtArgVal *value) ;
#endif /* _NO_PROTO */

#endif /* _InteractorP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
