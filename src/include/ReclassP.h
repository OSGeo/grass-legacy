#ifndef _ReclassP_h
#define _ReclassP_h

#include <InteractP.h>
#include <Reclass.h>
#include <gis.h>

#ifndef XmRECLASS_BIT
#define XmRECLASS_BIT     (54)
#endif

typedef struct _reclass_rule {
    int numOriginalCats;
    CELL *originalCats;
    CELL newCat;
    char *newLabel;
    struct _reclass_rule *next;
} XgReclassRuleRec, *XgReclassRule;

/* New fields for the Reclass widget class record  */

typedef struct {
    caddr_t                         extension;  /* Pointer to extension
                                                 * record */
}                               ReclassClassPart;


/* Full class record declaration */

typedef struct _ReclassClassRec {
    CoreClassPart                   core_class;
    CompositeClassPart              composite_class;
    ConstraintClassPart             constraint_class;
    XmManagerClassPart              manager_class;
    XmBulletinBoardClassPart        bulletin_board_class;
    InteractorClassPart             interactor_class;
    ReclassClassPart                reclass_class;
}                               ReclassClassRec;

externalref ReclassClassRec     reclassClassRec;


/* New fields for the Reclass widget record */

typedef struct {

    String                          original_map;
    String                          original_mapset;

    String                          new_map;
    String                          new_mapset;

    String                          new_map_title;

    Widget                          orig_map_name_form;
    Widget                          orig_map_name_form_frame;
    Widget                          new_map_title_form;
    Widget                          new_map_title_form_frame;
    Widget                          new_map_name_form;
    Widget                          new_map_name_form_frame;

    Widget                          original_map_label;
    XmString                        original_map_label_string;

    Widget                          new_map_title_label;
    XmString                        new_map_title_label_string;

    Widget                          new_map_label;
    XmString                        new_map_label_string;

    Widget                          original_map_text;
    String                          original_map_text_string;

    Widget                          new_map_title_text;
    String                          new_map_title_text_string;

    Widget                          new_map_text;
    String                          new_map_text_string;

    struct Categories               original_categories;
    struct Range                    original_range;

    Widget                          original_map_list_rc;

    Widget                          original_map_list_label;
    XmString                        original_map_list_label_string;

    Widget                          original_map_list;
    XmStringTable                   original_map_list_items;
    CELL                           *original_map_list_cats;
    int                             original_map_list_item_count;
    int                             original_map_list_visible_item_count;

    XgReclassRule                   new_reclass;

    Widget                          new_map_list_rc;

    Widget                          new_map_list_label;
    XmString                        new_map_list_label_string;

    Widget                          new_map_list;
    XmStringTable                   new_map_list_items;
    int                             new_map_list_item_count;
    int                             new_map_list_visible_item_count;

    Widget                          category_value_label;
    XmString                        category_value_label_string;

    Widget                          category_value_increment_frame;
    Widget                          category_value_increment_form;
    Widget                          category_value_increment_up;
    Widget                          category_value_increment_down;

    CELL                            category_value_increment_value;
    Widget                          category_value_increment_text;
    String                          category_value_increment_text_string;

    Widget                          category_name_text_label;
    XmString                        category_name_text_label_string;

    Widget                          category_name_text;
    String                          category_name_text_string;

    Widget                          category_group_button;

    Widget                          category_ungroup_button;
    Widget			    reclass_stretch_label;


    /* should we perform the reclass on OK and Apply automatically? */
    Boolean                         reclass_on_ok;
    Boolean                         reclass_on_apply;
}                               ReclassPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _ReclassRec {
    CorePart                        core;
    CompositePart                   composite;
    ConstraintPart                  constraint;
    XmManagerPart                   manager;
    XmBulletinBoardPart             bulletin_board;
    InteractorPart                  interactor;
    ReclassPart                     reclass;
}                               ReclassRec;


/* Access macros */

#define R_OriginalMapNameForm(w) \
 (((ReclassWidget) (w))->reclass.orig_map_name_form)
#define R_OriginalMapNameFormFrame(w) \
 (((ReclassWidget) (w))->reclass.orig_map_name_form_frame)
#define R_NewMapTitleForm(w) \
 (((ReclassWidget) (w))->reclass.new_map_title_form)
#define R_NewMapTitleFormFrame(w) \
 (((ReclassWidget) (w))->reclass.new_map_title_form_frame)
#define R_NewMapNameForm(w) \
 (((ReclassWidget) (w))->reclass.new_map_name_form)
#define R_NewMapNameFormFrame(w) \
 (((ReclassWidget) (w))->reclass.new_map_name_form_frame)
#define R_OriginalMapListRC(w) \
 (((ReclassWidget) (w))->reclass.original_map_list_rc)
#define R_NewMapListRC(w) \
 (((ReclassWidget) (w))->reclass.new_map_list_rc)
#define R_OriginalMap(w) \
 (((ReclassWidget) (w))->reclass.original_map)
#define R_OriginalMapset(w) \
 (((ReclassWidget) (w))->reclass.original_mapset)
#define R_OriginalMapLabel(w) \
 (((ReclassWidget) (w))->reclass.original_map_label)
#define R_OriginalMapLabelString(w) \
 (((ReclassWidget) (w))->reclass.original_map_label_string)
#define R_NewMap(w) \
 (((ReclassWidget) (w))->reclass.new_map)
#define R_NewMapset(w) \
 (((ReclassWidget) (w))->reclass.new_mapset)
#define R_NewMapTitle(w) \
 (((ReclassWidget) (w))->reclass.new_map_title)
#define R_NewMapTitleLabel(w) \
 (((ReclassWidget) (w))->reclass.new_map_title_label)
#define R_NewMapTitleLabelString(w) \
 (((ReclassWidget) (w))->reclass.new_map_title_label_string)
#define R_NewMapLabel(w) \
 (((ReclassWidget) (w))->reclass.new_map_label)
#define R_NewMapLabelString(w) \
 (((ReclassWidget) (w))->reclass.new_map_label_string)
#define R_OriginalMapText(w) \
 (((ReclassWidget) (w))->reclass.original_map_text)
#define R_OriginalMapTextString(w) \
 (((ReclassWidget) (w))->reclass.original_map_text_string)
#define R_NewMapTitleText(w) \
 (((ReclassWidget) (w))->reclass.new_map_title_text)
#define R_NewMapTitleTextString(w) \
 (((ReclassWidget) (w))->reclass.new_map_title_text_string)
#define R_NewMapText(w) \
 (((ReclassWidget) (w))->reclass.new_map_text)
#define R_NewMapTextString(w) \
 (((ReclassWidget) (w))->reclass.new_map_text_string)
#define R_OriginalMapListLabel(w) \
 (((ReclassWidget) (w))->reclass.original_map_list_label)
#define R_OriginalMapListLabelString(w) \
 (((ReclassWidget) (w))->reclass.original_map_list_label_string)
#define R_NewMapListLabel(w) \
 (((ReclassWidget) (w))->reclass.new_map_list_label)
#define R_NewMapListLabelString(w) \
 (((ReclassWidget) (w))->reclass.new_map_list_label_string)
#define R_OriginalCategories(w) \
 (((ReclassWidget) (w))->reclass.original_categories)
#define R_OriginalRange(w) \
 (((ReclassWidget) (w))->reclass.original_range)
#define R_OriginalMapList(w) \
 (((ReclassWidget) (w))->reclass.original_map_list)
#define R_OriginalMapListItems(w) \
 (((ReclassWidget) (w))->reclass.original_map_list_items)
#define R_OriginalMapListCats(w) \
 (((ReclassWidget) (w))->reclass.original_map_list_cats)
#define R_OriginalMapListItemCount(w) \
 (((ReclassWidget) (w))->reclass.original_map_list_item_count)
#define R_OriginalMapListVisibleItemCount(w) \
 (((ReclassWidget) (w))->reclass.original_map_list_visible_item_count)
#define R_NewReclass(w) \
 (((ReclassWidget) (w))->reclass.new_reclass)
#define R_NewMapList(w) \
 (((ReclassWidget) (w))->reclass.new_map_list)
#define R_NewMapListItems(w) \
 (((ReclassWidget) (w))->reclass.new_map_list_items)
#define R_NewMapListItemCount(w) \
 (((ReclassWidget) (w))->reclass.new_map_list_item_count)
#define R_NewMapListVisibleItemCount(w) \
 (((ReclassWidget) (w))->reclass.new_map_list_visible_item_count)
#define R_CategoryValueLabel(w) \
 (((ReclassWidget) (w))->reclass.category_value_label)
#define R_CategoryValueLabelString(w) \
 (((ReclassWidget) (w))->reclass.category_value_label_string)
#define R_CategoryValueIncrementFrame(w) \
 (((ReclassWidget) (w))->reclass.category_value_increment_frame)
#define R_CategoryValueIncrementForm(w) \
 (((ReclassWidget) (w))->reclass.category_value_increment_form)
#define R_CategoryValueIncrementUp(w) \
 (((ReclassWidget) (w))->reclass.category_value_increment_up)
#define R_CategoryValueIncrementDown(w) \
 (((ReclassWidget) (w))->reclass.category_value_increment_down)
#define R_CategoryValueIncrementValue(w) \
 (((ReclassWidget) (w))->reclass.category_value_increment_value)
#define R_CategoryValueIncrementText(w) \
 (((ReclassWidget) (w))->reclass.category_value_increment_text)
#define R_CategoryValueIncrementTextString(w) \
 (((ReclassWidget) (w))->reclass.category_value_increment_text_string)
#define R_CategoryNameTextLabel(w) \
 (((ReclassWidget) (w))->reclass.category_name_text_label)
#define R_CategoryNameTextLabelString(w) \
 (((ReclassWidget) (w))->reclass.category_name_text_label_string)
#define R_CategoryNameText(w) \
 (((ReclassWidget) (w))->reclass.category_name_text)
#define R_CategoryNameTextString(w) \
 (((ReclassWidget) (w))->reclass.category_name_text_string)
#define R_CategoryGroupButton(w) \
 (((ReclassWidget) (w))->reclass.category_group_button)
#define R_CategoryUngroupButton(w) \
 (((ReclassWidget) (w))->reclass.category_ungroup_button)
#define R_CategoryStretchLabel(w) \
 (((ReclassWidget) (w))->reclass.reclass_stretch_label)
#define R_ReclassOnOK(w) \
 (((ReclassWidget) (w))->reclass.reclass_on_ok)
#define R_ReclassOnApply(w) \
 (((ReclassWidget) (w))->reclass.reclass_on_apply)

#endif                          /* _ReclassP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
