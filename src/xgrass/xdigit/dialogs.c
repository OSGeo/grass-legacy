/******************************************************************************
 Keeps track of dialog boxes and closes all open dialogs when main menu 
 changes. dialogs is an array of lists of dialogs, one for each menu.

 add_dlog_list(wlist) adds the list of dialogs, wlist, to the array dialogs.

 clear_dlogs() called each time there is a menu change. calls alldown
	       for each widget list, which closes all dialogs

 alldown() unmanages a list of widgets	 

 init_dlogs() initializes each element of array dialogs to null.A

*******************************************************************************/



#include "digit.h"
#include <stdio.h>

#define NUM_MENUS 7

Widget *dialogs[NUM_MENUS];

void 
add_dlog_list (wlist)
    Widget *wlist;
{
    static int i=0;

    if (i < NUM_MENUS)
	dialogs[i++] = wlist;
    else 
	fprintf (stderr, "More dialog lists than menus\n");
}
void 
clear_dlogs()
{
    int i;

    for (i = 0; i < NUM_MENUS; i++)
    {
	if (dialogs[i] != NULL) 	
	{
	    alldown (NULL, dialogs[i], NULL);
	}
    }
}

void 
init_dlogs()
{
    int i;

    for (i = 0; i < NUM_MENUS; i++)
    {
	dialogs[i] = NULL;
    }
}

/* alldown: makes sure a list of widgets are all unmanaged
   (for instance, will pop down a list of dialogs when menu changes) */
void
alldown (w, wlist, call_data)
    Widget w, *wlist;
    caddr_t   call_data;
{
    int i;
    if (wlist != NULL)
    {
        for (i = 0; wlist[i] != NULL; i++)
            if (XtIsManaged (wlist[i]))
                XtUnmanageChild (wlist[i]);
    }
}
