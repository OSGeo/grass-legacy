/*
**  Written by Dave Gerdes  3/1988
**  US Army Construction Engineering Research Lab
*/

/*  global_menu (chr, M_from)
**
**  If calling routine has input char it does not recognize, then it
**    passes it to global_menu.  
**
**  RETURNS:
**     -1    Command not found or not enabled
**      0    Finished successfully
**     >0    Index into Main_subs[] to execute
**
*/
#include <stdio.h>
#include <string.h>
#include "debug.h"
#include "raster.h"
#include "dig_curses.h"
#include "keyboard.h"
#include "digit.h"
#include "Map_proto.h"
#include "local_proto.h"


#include "menus.i"

int 
_is_enabled (int chr, struct Menu_head *menu)
{
    register int i;

    for (i = 0 ; menu->item[i].text != NULL ; i++)
	if (menu->item[i].command == chr)
	    return (menu->item[i].enabled);
    return 0;
}

int 
global_menu (int chr, struct Menu_head *from)
{
    if (!_is_enabled (chr, &M_global))
    {
	return (-1);
    }
	
    switch (chr) {
	 case MGC_DIGIT:
	    return (MGI_DIGIT);
	    break;
	 case MGC_EDIT:
	    return (MGI_EDIT);
	    break;
	 case MGC_LABEL:
	    return (MGI_LABEL);
	    break;
	 case MGC_CUSTOM:
	    Customize ();
	    break;
	 case MGC_TOOL:
	    Toolbox ();
	    break;
	 case MGC_WINDOW:
	    Window ();
	    break;
	 case MGC_REDRAW:
	    clear_window ();
	    replot (CMap);
	    break;
	 case MGC_HELP:
	    Help (from);
	    break;
	 case MGC_REFRESH:
	    Replot_screen ();
	    break;
	 case MGC_ZOOM:
	    zoom_window (0, NULL);
	    break;
	 case MGC_DEBUG:
	    Debug ();
	    break;
	 case MGC_BREAK:
    /* there is a bug in the sunview driver.  when the screen saver
    ** clears the screen, the pipe is blocked and the window cannot receive
    ** new signals, so we close the pipe and reopen it if user presses the
    **  '^' key in digit
    */
	    R_close_driver ();	/* break the pipe and reopen it */
	    R_open_driver ();
	    break;
	 case MGC_QUIT:
	    _Clear_info ();
	    if (curses_yes_no_default (2, "Leave digit? ", 0))
		return (MGI_QUIT);
	    _Clear_info ();
	    break;
	default:
	    return (-1);
	    break;
    }
    return (0);
}

int 
update_global_menu (void)
{
    _update_global_menu ();
    _Info_refresh ();
    return 0;
}

int 
_update_global_menu (void)
{
    char str[512];
    register int i;

    _Clear_info ();
    strcpy (str, " ");
    for (i = 0 ;  M_global.item[i].text != NULL ; i++)
	if (M_global.item[i].enabled)
	    strcat (str, M_global.item[i].text);
    _Write_info (1, str);
    if (!Terse_On)
    _Write_info (4, "GLOBAL MENU: Press first letter of desired command. [Upper Case Only]");
    return 0;
}

/* display Menu head and options if activated */
int 
update_menu (struct Menu_head *menuhead)
{
    register int baseline;
    char tmpstr[512];
    register int i;
    int linecnt = 0;
    int startcol = 1;

    if (menuhead == &M_digit)
	baseline = 10;
    else
	baseline = 3;
    sprintf (tmpstr, " %s options:", menuhead->name);
    _Write_base (baseline++, tmpstr);

    for (i = 0 ;  menuhead->item[i].text != NULL ; i++)
    {
	if (linecnt == 14)
	{
	    baseline = 4;
	    startcol = 39;
	}
	if (menuhead->item[i].enabled != (char) -1) /* dont display */
	{
	    if (menuhead->item[i].enabled)
	    {
		sprintf (tmpstr, "  %s", menuhead->item[i].text);
		_Base_string (baseline++, startcol, tmpstr);
	    }
	    else	/* this is for debugging */
	    {
		sprintf (tmpstr, "  %s", menuhead->item[i].text);
		if (tmpstr[2]) tmpstr[2] = ' ';
		_Base_string (baseline++, startcol, tmpstr);
	    }
	    linecnt++;
	}
    }
    return 0;
}

int 
get_menu_command (struct Menu_head *menu, int *chr)
{
    int command;
    register int i;

/*DEBUG*/   /* trying to overcome screen lockup problem */
/*DEBUG     flush_keyboard () */
    command = curses_getchar () & 0177;
    for (i = 0 ; menu->item[i].text != NULL ; i++)
	if (menu->item[i].enabled && command == menu->item[i].command)
	    return (command);
    *chr = command;
    return (0);
}

int 
set_menu_item (struct Menu_head *menu, int item, int onoff)
{
    menu->item[item].enabled = onoff && Global_mask.item[item].enabled;
    return 0;
}

int 
Set_Global (int item, int onoff)
{
    set_menu_item (&M_global, item, onoff);
    return 0;
}

int 
Set_G_Mask (int item, int onoff)
{
    Global_mask.item[item].enabled = onoff;
    M_global.item[item].enabled = onoff;
    return 0;
}

/* these are defunct for now.  maybe later they can be used for extended help */
int Help_main (void) {return 0;}
int Help_global (void) {return 0;}
int Help_digitize (void) {return 0;}
int Help_edit (void) {return 0;}
int Help_label (void) {return 0;}
int Help_window (void) {return 0;}
int Help_custom (void) {return 0;}
int Help_tool (void) {return 0;}
int Help_display (void) {return 0;}
int Help_debug (void) {return 0;}
int Help_color (void) {return 0;}
