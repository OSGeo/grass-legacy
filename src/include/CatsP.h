/*
 * File: CatsP.h
 *
 * Desc: Private interface for Cats widget
 *
 * Auth: Eric W. Sink
 *
 * Date: 24 Feb 1992
 *
 * Modification History:
 *
 *
 */

#ifndef _CatsP_h
#define _CatsP_h

#include <InteractP.h>
#include <Cats.h>

#ifndef XmCATS_BIT
#define XmCATS_BIT     (56)
#endif

/* New fields for the Cats widget class record  */

typedef struct {
    caddr_t         extension;  /* Pointer to extension record */
}               CatsClassPart;


/* Full class record declaration */

typedef struct _CatsClassRec {
    CoreClassPart   core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmBulletinBoardClassPart bulletin_board_class;
    InteractorClassPart interactor_class;
    CatsClassPart   cats_class;
}               CatsClassRec;

externalref CatsClassRec catsClassRec;


/* New fields for the Cats widget record */

typedef struct {
    Widget          title_label;
    Widget          title_text;
    Widget          categories_label;
    Widget          categories_list;
    Widget          add_button;
    Widget          delete_button;
    Widget          name_text;
    Widget          list_container;
    Widget          lower_container;
    Widget          buttons_container;
    Widget          title_separator;
    XmStringTable   cat_items;
    int             cat_items_count;

    struct Range   *myRange;
    struct Categories *myCats;
    int             current_cat;
    int             current_cat_num;

    Widget          category_value_label;
    Widget          category_name_label;
    Widget          category_name_frame;
    Widget          category_name_form;
    XmString        category_value_label_string;

    Widget          category_value_increment_frame;
    Widget          category_value_increment_form;
    Widget          category_value_increment_up;
    Widget          category_value_increment_down;

    int             category_value_increment_value;
    Widget          category_value_increment_text;
    String          category_value_increment_text_string;

    /* display area */
    Widget          display_area;
}               CatsPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _CatsRec {
    CorePart        core;
    CompositePart   composite;
    ConstraintPart  constraint;
    XmManagerPart   manager;
    XmBulletinBoardPart bulletin_board;
    InteractorPart  interactor;
    CatsPart        cats;
}               CatsRec;


#define XgCATS_DISPLAY_AREA  0

/* Access macros */

#define C_DisplayArea( w) (((CatsWidget) (w))->cats.display_area)

#define C_CategoryValueLabel(w) \
 (((CatsWidget) (w))->cats.category_value_label)
#define C_CategoryValueLabelString(w) \
 (((CatsWidget) (w))->cats.category_value_label_string)
#define C_CategoryValueIncrementFrame(w) \
 (((CatsWidget) (w))->cats.category_value_increment_frame)
#define C_CategoryValueIncrementForm(w) \
 (((CatsWidget) (w))->cats.category_value_increment_form)
#define C_CategoryValueIncrementUp(w) \
 (((CatsWidget) (w))->cats.category_value_increment_up)
#define C_CategoryValueIncrementDown(w) \
 (((CatsWidget) (w))->cats.category_value_increment_down)
#define C_CategoryValueIncrementValue(w) \
 (((CatsWidget) (w))->cats.category_value_increment_value)
#define C_CategoryValueIncrementText(w) \
 (((CatsWidget) (w))->cats.category_value_increment_text)
#define C_CategoryValueIncrementTextString(w) \
 (((CatsWidget) (w))->cats.category_value_increment_text_string)
#define C_MyCats( w) (((CatsWidget) (w))->cats.myCats)
#define C_MyRange( w) (((CatsWidget) (w))->cats.myRange)
#define C_TitleLabel( w) (((CatsWidget) (w))->cats.title_label)
#define C_TitleText( w) (((CatsWidget) (w))->cats.title_text)
#define C_CategoriesList( w) (((CatsWidget) (w))->cats.categories_list)
#define C_CategoriesLabel( w) (((CatsWidget) (w))->cats.categories_label)
#define C_AddButton( w) (((CatsWidget) (w))->cats.add_button)
#define C_DeleteButton( w) (((CatsWidget) (w))->cats.delete_button)
#define C_NameLabel( w) (((CatsWidget) (w))->cats.category_name_label)
#define C_NameForm( w) (((CatsWidget) (w))->cats.category_name_form)
#define C_NameFrame( w) (((CatsWidget) (w))->cats.category_name_frame)
#define C_NameText( w) (((CatsWidget) (w))->cats.name_text)
#define C_ListContainer( w) (((CatsWidget) (w))->cats.list_container)
#define C_LowerContainer( w) (((CatsWidget) (w))->cats.lower_container)
#define C_ButtonsContainer( w) (((CatsWidget) (w))->cats.buttons_container)
#define C_TitleSeparator( w) (((CatsWidget) (w))->cats.title_separator)
#define C_CatItems( w) (((CatsWidget) (w))->cats.cat_items)
#define C_CatItemsCount( w) (((CatsWidget) (w))->cats.cat_items_count)
#define C_CurrentCat( w) (((CatsWidget) (w))->cats.current_cat)
#define C_CurrentCatNum( w) (((CatsWidget) (w))->cats.current_cat_num)

#endif                          /* _CatsC_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
