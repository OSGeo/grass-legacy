/*
 * File: menu_item.h
 *
 * Desc: contains defs for pull down menu system
 *
 * Auth: Kurt Buehler
 *
 * Date:
 *
 * Modification History:
 *
 *
 */
#ifndef MENU_ITEM_H
#define MENU_ITEM_H

#define XG_MENU_NONE       0
#define XG_MENU_PULLDOWN   1
#define XG_MENU_PULLRIGHT  2
#define XG_MENU_TEAR_OFF_H 3
#define XG_MENU_TEAR_OFF_V 4
#define XG_MENU_HACK       5 /* don't ask */

typedef struct _xg_menu_item {
    char *label;             /* the label for the item */
    char *parent_label;      /* the label for the items' parent */
    WidgetClass *class;      /* pushbutton, cascade, label, separator... */
    char *mnemonic;         /* mnemonic; NULL if none */
    char *accelerator;       /* accelerator; NULL if none */
    char *accel_text;        /* to be converted to compound string */
    void (*callback)();      /* routine to call; NULL if none */
    char *arglist;           /* arglist for function */
    XtPointer callback_data; /* client_data for callback() */
    int submenu_type;        /* XG_MENU_PULLRIGHT, 
				XG_xMENU_TEAR_OFF, XG_MENU_NONE */
    int subitem_count;       /* the number of subitems */
    struct _xg_menu_item *subitems; 
     /* pullright, option, or tear-off menu items, if not NULL */
    char *submenu_label;     /* submenu label (expected) */
} XgMenuItemRec;

typedef struct _xg_menu_item_list {
    XgMenuItemRec *menu_item;
    struct _xg_menu_item_list *next;
} XgMenuItemListRec;

typedef struct _xg_menu_bar_list {
    XgMenuItemRec *menubar_item;
    struct _xg_menu_bar_list *next;
} XgMenuBarListRec;

typedef struct _xg_menu_data {
    char *specFile;             /* path to specification file */
    XgMenuBarListRec *menuBarList; /* list of main menubar definitions */
} XgMenuData;

#endif /* _MENU_ITEM_H */
