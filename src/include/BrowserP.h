/*
 * File: BrowserP.h
 *
 * Desc: Private interface file for Browser
 *
 * Auth: Eric W. Sink
 *
 * Date: 24 Feb 1992
 *
 * Modification History:
 *
 *
 */

#ifndef _BrowserP_h
#define _BrowserP_h

#include <InteractP.h>
#include <Browser.h>

#ifndef XmBROWSER_BIT
#define XmBROWSER_BIT     (51)
#endif

/* New fields for the Browser widget class record  */

typedef struct {
    caddr_t                         extension;  /* Pointer to extension
                                                 * record */
}                               BrowserClassPart;


/* Full class record declaration */

typedef struct _BrowserClassRec {
    CoreClassPart                   core_class;
    CompositeClassPart              composite_class;
    ConstraintClassPart             constraint_class;
    XmManagerClassPart              manager_class;
    XmBulletinBoardClassPart        bulletin_board_class;
    InteractorClassPart             interactor_class;
    BrowserClassPart                browser_class;
}                               BrowserClassRec;

externalref BrowserClassRec     browserClassRec;


/* New fields for the Browser widget record */

typedef struct {
    int                             num_lists;

    XmString                        result_string;

    Boolean                         user_defined;
    char                           *db_element;

    /* These contain arrays of strings of items in each list */
    Boolean			    list_all_mapsets;
    char                          **list1_items;
    char                          **list2_items;
    char                          **list3_items;
    int                             list1_count;
    int                             list2_count;
    int                             list3_count;
    Boolean                         list1_isstatic;
    Boolean                         list2_isstatic;
    Boolean                         list3_isstatic;

    /* The three list children */
    Widget                          list1;
    Widget                          list2;
    Widget                          list3;

    /*
     * menu1_which gives the index of the currently selected map in the
     * option menu by list 1
     */
    int                             menu1_which;
    int                             menu2_which;
    int                             menu3_which;

    /* Selects the kind of item we are browsing. */
    int                             browse_mode;
    /* Selects multi/single selection mode */
    int                             sel_mode;

    /* Cascade buttons for the 3 option menus */
    Widget                          menu1_button;
    Widget                          menu2_button;
    Widget                          menu3_button;
    /* The 3 option menus */
    Widget                          menu1;
    Widget                          menu2;
    Widget                          menu3;

    Widget                          between1;
    Widget                          between2;
    Widget                          between3;

    Widget                          list1_container;
    Widget                          list2_container;
    Widget                          list3_container;

    int                             options_count;      /* number of valid
                                                         * mapsets */
    char                          **options_items;

    XmString                        initial_mapset1;
    XmString                        initial_mapset2;
    XmString                        initial_mapset3;

    /* display area */
    Widget                          display_area;
}                               BrowserPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _BrowserRec {
    CorePart                        core;
    CompositePart                   composite;
    ConstraintPart                  constraint;
    XmManagerPart                   manager;
    XmBulletinBoardPart             bulletin_board;
    InteractorPart                  interactor;
    BrowserPart                     browser;
}                               BrowserRec;


#define XgLIST1 1
#define XgLIST2 2
#define XgLIST3 3

#define XgBROWSER_DISPLAY_AREA  0

/* Access macros */

#define B_DisplayArea( w) (((BrowserWidget) (w))->browser.display_area)
#define B_ListAllMapsets( w) (((BrowserWidget) (w))->browser.list_all_mapsets)
#define B_NumLists( w) (((BrowserWidget) (w))->browser.num_lists)
#define B_List1( w) (((BrowserWidget) (w))->browser.list1)
#define B_List2( w) (((BrowserWidget) (w))->browser.list2)
#define B_List3( w) (((BrowserWidget) (w))->browser.list3)
#define B_List1Items( w) (((BrowserWidget) (w))->browser.list1_items)
#define B_List2Items( w) (((BrowserWidget) (w))->browser.list2_items)
#define B_List3Items( w) (((BrowserWidget) (w))->browser.list3_items)
#define B_List1Count( w) (((BrowserWidget) (w))->browser.list1_count)
#define B_List2Count( w) (((BrowserWidget) (w))->browser.list2_count)
#define B_List3Count( w) (((BrowserWidget) (w))->browser.list3_count)
#define B_List1Container( w) (((BrowserWidget) (w))->browser.list1_container)
#define B_List2Container( w) (((BrowserWidget) (w))->browser.list2_container)
#define B_List3Container( w) (((BrowserWidget) (w))->browser.list3_container)
#define B_List1IsStatic( w) (((BrowserWidget) (w))->browser.list1_isstatic)
#define B_List2IsStatic( w) (((BrowserWidget) (w))->browser.list2_isstatic)
#define B_List3IsStatic( w) (((BrowserWidget) (w))->browser.list3_isstatic)
#define B_InitialMapset1( w) (((BrowserWidget) (w))->browser.initial_mapset1)
#define B_InitialMapset2( w) (((BrowserWidget) (w))->browser.initial_mapset2)
#define B_InitialMapset3( w) (((BrowserWidget) (w))->browser.initial_mapset3)
#define B_Between1( w) (((BrowserWidget) (w))->browser.between1)
#define B_Between2( w) (((BrowserWidget) (w))->browser.between2)
#define B_Between3( w) (((BrowserWidget) (w))->browser.between3)
#define B_Menu1( w) (((BrowserWidget) (w))->browser.menu1)
#define B_Menu2( w) (((BrowserWidget) (w))->browser.menu2)
#define B_Menu3( w) (((BrowserWidget) (w))->browser.menu3)
#define B_Menu1Button( w) (((BrowserWidget) (w))->browser.menu1_button)
#define B_Menu2Button( w) (((BrowserWidget) (w))->browser.menu2_button)
#define B_Menu3Button( w) (((BrowserWidget) (w))->browser.menu3_button)
#define B_Menu1Which( w) (((BrowserWidget) (w))->browser.menu1_which)
#define B_Menu2Which( w) (((BrowserWidget) (w))->browser.menu2_which)
#define B_Menu3Which( w) (((BrowserWidget) (w))->browser.menu3_which)
#define B_ResultString( w) (((BrowserWidget) (w))->browser.result_string)
#define B_BrowseMode( w) (((BrowserWidget) (w))->browser.browse_mode)
#define B_SelMode( w) (((BrowserWidget) (w))->browser.sel_mode)
#define B_OptionsItems( w) (((BrowserWidget) (w))->browser.options_items)
#define B_OptionsCount( w) (((BrowserWidget) (w))->browser.options_count)
#define B_UserDefined( w) (((BrowserWidget) (w))->browser.user_defined)
#define B_DBElement( w) (((BrowserWidget) (w))->browser.db_element)

#endif                          /* _BrowserB_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
