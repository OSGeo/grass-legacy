static char rcsid[] = "@(#)XGRASS $Id: link.c,v 0.0.0.1 1992/05/05 14:58:37 kurt Exp kurt $";
/*
 * File:
 *
 * Desc:
 *
 * Auth:
 *
 * Date:
 *
 * Modification History:
 *
 *
 */

#include "xgrass.h"

void
LinkMenuSystem(mblist, milist)
XgMenuBarListRec *mblist;
XgMenuItemListRec *milist;
{
    XgMenuBarListRec *mbptr = mblist;
    XgMenuItemListRec *miptr = milist;

    /* count then connect menu bar sub menus */
    while ( mbptr ) {
        miptr = milist;
        while ( miptr ) {
            if ( mbptr->menubar_item->submenu_label &&
                !strcmp(mbptr->menubar_item->submenu_label,
                miptr->menu_item->parent_label)) {
                mbptr->menubar_item->subitem_count++;
            }
            miptr = miptr->next;
        }
        mbptr = mbptr->next;
    }
    mbptr = mblist;
    while ( mbptr ) {
        int count = 0;

        mbptr->menubar_item->subitems = (XgMenuItemRec *)XtCalloc(
            mbptr->menubar_item->subitem_count, sizeof(XgMenuItemRec));
        miptr = milist;
        while ( miptr ) {
            if ( mbptr->menubar_item->submenu_label &&
                !strcmp(mbptr->menubar_item->submenu_label,
                miptr->menu_item->parent_label)) {
                bcopy((char *)miptr->menu_item,
                    (char *)&mbptr->menubar_item->subitems[count++], 
                    sizeof(XgMenuItemRec));
            }
            miptr = miptr->next;
        }
        mbptr = mbptr->next;
    }
    /* now count then connect sub menu items */
    CountItems(milist);
    LinkItems(mblist,milist);
    /*
    mbptr = mblist;
    while( mbptr ) {
        fprintf(stderr,"menubar item: %s\n",mbptr->menubar_item->label);
        PrintItems(mbptr->menubar_item,"\t");
        mbptr = mbptr->next;
    }
    */
    FreeItems(milist);
}

CountItems(list)
XgMenuItemListRec *list;
{
    XgMenuItemListRec *ptr;

    ptr = list;
    while ( ptr ) {
	if ( ptr->menu_item != XG_MENU_NONE ) 
            CountSubItems(ptr->menu_item, list);
        ptr = ptr->next;
    }
}

CountSubItems(item, list)
XgMenuItemRec *item;
XgMenuItemListRec *list;
{
    XgMenuItemListRec *ptr = list;

    ptr = list;
    while ( ptr ) {
        if ( item->submenu_label && !strcmp(item->submenu_label,
            ptr->menu_item->parent_label)) {
            item->subitem_count++;
        }
        ptr = ptr->next;
    }
}

LinkItems(mblist,list)
XgMenuBarListRec *mblist;
XgMenuItemListRec *list;
{
    XgMenuBarListRec *ptr;

    ptr = mblist;
    while ( ptr ) {
	if ( ptr->menubar_item->submenu_type != XG_MENU_NONE ) 
            LinkSubItems(ptr->menubar_item, list);
        ptr = ptr->next;
    }
}

LinkSubItems(item,list)
XgMenuItemRec *item;
XgMenuItemListRec *list;
{
    int count = 0;
    XgMenuItemListRec *ptr = list;


    if ( item->subitem_count ) {
        item->subitems = (XgMenuItemRec *)
	    XtCalloc(item->subitem_count, sizeof(XgMenuItemRec));
        ptr = list;
        while ( ptr ) {
    
            if ( !strcmp(item->submenu_label,ptr->menu_item->parent_label)) {
                bcopy((char *)ptr->menu_item, (char *)&item->subitems[count], 
                      sizeof(XgMenuItemRec));
		/* set callback_data pointer */
		if ( item->subitems[count].callback ) 
		    item->subitems[count].callback_data = 
			(XtPointer)&item->subitems[count];
		if ( item->subitems[count].submenu_type != XG_MENU_NONE ) {
		   LinkSubItems(&item->subitems[count],list);
		}
		count++;
            }
            ptr = ptr->next;
        }
    }
}

FreeItems(list)
XgMenuItemListRec *list;
{
    XgMenuItemListRec *ptr;

    ptr = list;
    if ( ptr->next ) {
        FreeItems(ptr->next);
    } else {
	XtFree(ptr);
    }
}

PrintItems(ptr,tabs)
XgMenuItemRec *ptr;
char *tabs;
{
    int i;

    if ( ptr->subitem_count > 0 ) {
        for ( i = 0; i < ptr->subitem_count; i++ ) {
            fprintf(stderr,"%ssubitem label: %s\n",tabs,ptr->subitems[i].label);
            if ( ptr->subitems[i].submenu_type != XG_MENU_NONE ) {
                char *newtabs = XtMalloc(strlen(tabs) + 2);
                strcpy(newtabs,tabs); 
                strcat(newtabs,"\t");
                PrintItems(&ptr->subitems[i],newtabs);
                XtFree(newtabs);
            }
        }
    } else
        fprintf(stderr,"%sno sub items\n",tabs);
}
