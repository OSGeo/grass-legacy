/*
 * File: HistoryP.h
 *
 * Desc: Private interface for History widget
 *
 * Auth: Eric W. Sink
 *
 * Date: 24 Feb 1992
 *
 * Modification History:
 *
 *
 */

#ifndef _HistoryP_h
#define _HistoryP_h

#include <InteractP.h>
#include <History.h>

#ifndef XmHISTORY_BIT
#define XmHISTORY_BIT     (55)
#endif

/* New fields for the History widget class record  */

typedef struct {
    caddr_t                         extension;  /* Pointer to extension
                                                 * record */
}                               HistoryClassPart;


/* Full class record declaration */

typedef struct _HistoryClassRec {
    CoreClassPart                   core_class;
    CompositeClassPart              composite_class;
    ConstraintClassPart             constraint_class;
    XmManagerClassPart              manager_class;
    XmBulletinBoardClassPart        bulletin_board_class;
    InteractorClassPart             interactor_class;
    HistoryClassPart                history_class;
}                               HistoryClassRec;

externalref HistoryClassRec     historyClassRec;


/* New fields for the History widget record */

typedef struct {
    Widget fieldsContainer;

    Widget mapidLabel;
    Widget titleLabel;
    Widget mapsetLabel;
    Widget creatorLabel;
    Widget maptypeLabel;
    Widget datsrc1Label;
    Widget datsrc2Label;
    Widget keywrdLabel;

    Widget mapidText;
    Widget titleText;
    Widget mapsetText;
    Widget creatorText;
    Widget maptypeText;
    Widget datsrc1Text;
    Widget datsrc2Text;
    Widget keywrdText;

    Widget edhistForm;
    Widget edhistDialog;
    Widget edhistButton;
    int edlinecnt;
    char *edhistString;
    Widget edhistLabel;
    Widget edhistText;

    struct History *myHistory;

    /* display area */
    Widget                          display_area;
}                               HistoryPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _HistoryRec {
    CorePart                        core;
    CompositePart                   composite;
    ConstraintPart                  constraint;
    XmManagerPart                   manager;
    XmBulletinBoardPart             bulletin_board;
    InteractorPart                  interactor;
    HistoryPart                     history;
}                               HistoryRec;


#define XgHISTORY_DISPLAY_AREA  0

/* Access macros */

#define H_DisplayArea( w) (((HistoryWidget) (w))->history.display_area)
#define H_FieldsContainer( w) (((HistoryWidget) (w))->history.fieldsContainer)
#define H_MapidText( w) (((HistoryWidget) (w))->history.mapidText)
#define H_TitleText( w) (((HistoryWidget) (w))->history.titleText)
#define H_MapsetText( w) (((HistoryWidget) (w))->history.mapsetText)
#define H_CreatorText( w) (((HistoryWidget) (w))->history.creatorText)
#define H_MaptypeText( w) (((HistoryWidget) (w))->history.maptypeText)
#define H_Datsrc1Text( w) (((HistoryWidget) (w))->history.datsrc1Text)
#define H_Datsrc2Text( w) (((HistoryWidget) (w))->history.datsrc2Text)
#define H_KeywrdText( w) (((HistoryWidget) (w))->history.keywrdText)

#define H_MapidLabel( w) (((HistoryWidget) (w))->history.mapidLabel)
#define H_TitleLabel( w) (((HistoryWidget) (w))->history.titleLabel)
#define H_MapsetLabel( w) (((HistoryWidget) (w))->history.mapsetLabel)
#define H_CreatorLabel( w) (((HistoryWidget) (w))->history.creatorLabel)
#define H_MaptypeLabel( w) (((HistoryWidget) (w))->history.maptypeLabel)
#define H_Datsrc1Label( w) (((HistoryWidget) (w))->history.datsrc1Label)
#define H_Datsrc2Label( w) (((HistoryWidget) (w))->history.datsrc2Label)
#define H_KeywrdLabel( w) (((HistoryWidget) (w))->history.keywrdLabel)

#define H_Edlinecnt( w) (((HistoryWidget) (w))->history.edlinecnt)
#define H_EdhistString( w) (((HistoryWidget) (w))->history.edhistString)
#define H_EdhistLabel( w) (((HistoryWidget) (w))->history.edhistLabel)
#define H_EdhistText( w) (((HistoryWidget) (w))->history.edhistText)
#define H_EdhistDialog( w) (((HistoryWidget) (w))->history.edhistDialog)
#define H_EdhistButton( w) (((HistoryWidget) (w))->history.edhistButton)
#define H_EdhistForm( w) (((HistoryWidget) (w))->history.edhistForm)
#define H_MyHistory( w) (((HistoryWidget) (w))->history.myHistory)

#endif                          /* _HistoryH_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
