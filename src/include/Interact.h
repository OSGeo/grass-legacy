#ifndef _Interact_h
#define _Interact_h

#include <Xm/Xm.h>

#ifndef XmINTERACTOR_BIT
#define XmINTERACTOR_BIT (49)
#endif

/* for GetChild */
#define XmINTERACT_APPLY_BUTTON    1
#define XmINTERACT_CANCEL_BUTTON   2
#define XmINTERACT_OK_BUTTON       3
#define XmINTERACT_HELP_BUTTON     4
#define XmINTERACT_LIST_TEXT       5
#define XmINTERACT_LIST_TEXT_LABEL 6
#define XmINTERACT_LIST            7
#define XmINTERACT_LIST_LABEL      8
#define XmINTERACT_PROMPT_LABEL    9
#define XmINTERACT_TEXT            10
#define XmINTERACT_SEPARATOR       11
#define XmINTERACT_WORK_AREA       12

/* dialog types */
#define XmINTERACT_WORK_AREA_TYPE 0
#define XmINTERACT_DIALOG_TYPE 1
#define XmINTERACT_PROMPT_TYPE    2
#define XmINTERACT_LIST_TYPE      3

typedef struct
{
    int     reason;
    XEvent  *event;
    XmString value;
    int     length;
} InteractorCallbackStruct;

/* This structure is used as *the* client_data to XgHelpCallback */
typedef struct _xg_help_struct {
Widget widget;
char *text;
struct _xg_help_struct *next;
} XgHelpStruct;


/* Class record constants */

externalref WidgetClass interactorWidgetClass;

typedef struct _InteractorClassRec * InteractorWidgetClass;
typedef struct _InteractorRec      * InteractorWidget;

#define XmNlistTextColumns "listTextColumns"
#define XmCListTextColumns "ListTextColumns"

#define XmNlistTextString "listTextString"
#define XmCListTextString "ListTextString"

#define XmNlistTextLabelString "listTextLabelString"
#define XmCListTextLabelString "ListTextLabelString"

#ifndef XmNlistLabelString
#define XmNlistLabelString "listLabelString"
#endif
#ifndef XmCListLabelString
#define XmCListLabelString "ListLabelString"
#endif

#define XmNpromptLabelString "promptLabelString"
#define XmCPromptLabelString "PromptLabelString"

#define XmNhelpItemLabels "helpItemLabels"
#define XmCHelpItemLabels "HelpItemLabels"

#define XmNhelpItems "helpItems"
#define XmCHelpItems "HelpItems"

#define XmNhelpItemCount "helpItemCount"
#define XmCHelpItemCount "HelpItemCount"

#define XmNenableWorkAreaStretch "enableWorkAreaStretch"
#define XmCEnableWorkAreaStretch "EnableWorkAreaStretch"

#ifndef XgIsInteractor
#define XgIsInteractor(w) (_XmIsFastSubclass(XtClass(w),XmINTERACTOR_BIT))
#endif


/*  
 * PUBLIC API PROTOTYPES
 */
#ifdef _NO_PROTO

extern Widget _XgInteractorCreateButtonG();
extern void _XgInteractorUpdateString() ;
extern void _XgInteractorHelpAction() ;
extern void _XgInteractorActivate() ;
extern Widget XgInteractorGetChild() ;
extern Widget XgCreateInteractor() ;
extern Widget XgCreateInteractorDialog() ;
extern Widget XgCreateInteractorPrompt() ;
extern Widget XgCreateInteractorPromptDialog() ;
extern Widget XgCreateInteractorList() ;
extern Widget XgCreateInteractorListDialog() ;
extern void XgHelpCallback() ;
extern Widget XgCreateInteractorFontListDialog();
extern Widget XgSetFontList();

#else /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

extern Widget XgSetFontList(
   Widget parent,
   Widget child
);

extern Widget XgCreateInteractorFontListDialog(
   Widget ds_p,       
   String name,       
   ArgList xgi_args,
   Cardinal xgi_n					       
);

extern Widget _XgInteractorCreateButtonG(
    Widget xgi, 
    XmString lstring, 
    char *name);

extern void _XgInteractorUpdateString(
    Widget w, 
    XmString string, 
    XmStringDirection direction
) ;

extern void _XgInteractorHelpAction(
    Widget widget,
    XEvent *event,
    String *parms,
    Cardinal nparms
) ;

extern void _XgInteractorActivate(
    Widget widget,
    XEvent *event,
    String *parms,
    Cardinal nparms
) ;

extern Widget XgInteractorGetChild( 
    Widget xgi, 
#if NeedWidePrototypes
    unsigned int   child
#else
    unsigned char   child
#endif 
) ;

extern Widget XgCreateInteractor( 
    Widget p, 
    String name, 
    ArgList args,
    Cardinal n
) ;

extern Widget XgCreateInteractorDialog( 
    Widget ds_p, 
    String name,
    ArgList xgi_args, 
    Cardinal xgi_n
) ;

extern Widget XgCreateInteractorPrompt( 
    Widget p, 
    String name, 
    ArgList args,
    Cardinal n
) ;

extern Widget XgCreateInteractorPromptDialog( 
    Widget ds_p, 
    String name,
    ArgList xgi_args, 
    Cardinal xgi_n
) ;

extern Widget XgCreateInteractorList( 
    Widget p, 
    String name, 
    ArgList args,
    Cardinal n
) ;

extern Widget XgCreateInteractorListDialog( 
    Widget ds_p, 
    String name,
    ArgList xgi_args, 
    Cardinal xgi_n
) ;

extern void XgHelpCallback(
    Widget w,
    XtPointer client_data,
    XtPointer call_data
) ;

#endif /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif


#endif /* _Interact_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
