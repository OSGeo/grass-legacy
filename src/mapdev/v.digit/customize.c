/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "dig_head.h"
#include "wind.h"

/*
**  Customize menu 
**
**  driver routine for Customize menu
**
*/
int
Customize ()
{
    int command;		/* command user enters */
    int ret;			/* get return values from subrs */
    int Pass;			/* Holds value to return to caller */
    int chr;

    int Changes= 0;

    Pass = 0;

    while(1) 
    {
	Set_Global (MG_DIGIT, OFF);
	Set_Global (MG_EDIT, OFF);
	Set_Global (MG_LABEL, OFF);
	Set_Global (MG_CUSTOM, OFF);
	Set_Global (MG_TOOL, OFF);
	Set_Global (MG_WINDOW, OFF);
	Set_Global (MG_QUIT, OFF);

	_Clear_info ();
	update_global_menu ();
	_Write_generic_win(&M_custom);
	Custom_settings (CM);
	_Base_refresh ();

	if ((command = get_menu_command (&M_custom, &chr)) > 0)
	{
	    switch(command) {
		case MCC_DTHRESH:
		    reset_thresh (CM);
		    Vect__write_head_binary (CM, &(CM->head));
		    Changes = 1;
		    break;
		case MCC_STHRESH:
		    reset_snap_thresh (CM);
		    Changes = 1;
		    break;
		case MCC_BEEP:
		    TOGGLE (Beep_On);
		    Changes = 1;
		    break;
/*
		case MCC_TERSE:
		    TOGGLE (Terse_On);
		    Changes = 1;
		    break;
*/
		case MCC_COMPRESS:
		    TOGGLE (Compress_File);
		    break;
/*
		case MCC_DEVICE:
		    TOGGLE (Label_Device);
		    Changes = 1;
		    break;
*/
		case MCC_WINDOW:
		    if (Dig_Enabled)
		    {
			TOGGLE (Window_Device);
			Changes = 1;
		    }
		    break;
		case MCC_POINT:
		    if (Dig_Enabled)
		    {
			TOGGLE (Point_Device);
			Changes = 1;
		    }
		    break;
		case MCC_DIGTIZ:
		    TOGGLE (Digtiz_Device);
		    Changes = 1;
		    break;
		case MCC_REMOVE_DRAW:
		    TOGGLE (Remove_Draw);
		    Changes = 1;
		    break;
		case MCC_BACKDROP:
		    {
			register int ret;
			suspend ();
			ret = ask_backdrop ();
			respend ();
			/* this (and overlay) should be modified
			** to replot on all changes, not just new map
			*/
			if (ret) 
			{
			    int save;

			    window_rout (U_north, U_south, U_east, U_west);
			    save = Disp_backdrop;
			    Disp_backdrop = 1;
			    clear_window ();
			    replot (CM);
			    Disp_backdrop = save;
			}
		    }
		    break;
		case MCC_OVERLAY:
		    {
			register int ret;
			suspend ();
			ret = ask_overlay ();
			respend ();
			if (ret) display_overlay ();
		    }
		    break;
		/*
		case MCC_LABEL:
		    {
			register int ret;
			suspend ();
			ret = ask_cust_label ();
			respend ();
			if (ret) display_cust_labels ();
		    }
		    break;
		*/
		case MCC_DISPLAY:
		    Changes = window_setup (CM);
		    break;
		case MCC_COLOR:
		    Changes = color_setup (CM);
		    break;
		case MCC_AUTOWIND:
		    TOGGLE (Auto_Window);
		    Changes = 1;
		    break;
		case MCC_QUIT:
		    Pass = 0;
		    goto CUST_END;
		    break;
		default:
		    break;
	    }
	}
	else
	{
	    if ((ret = global_menu (chr, &M_custom)) > 0)
	    {
		Pass = ret;	/* should never get here for customize () */
		break;  /* return and execute new command */
	    }
	    if (ret < 0)
		BEEP;
	}
    }
CUST_END:
    Set_Global (MG_DIGIT, ON);
    Set_Global (MG_EDIT, ON);
    Set_Global (MG_LABEL, ON);
    Set_Global (MG_CUSTOM, ON);
    Set_Global (MG_TOOL, ON);
    Set_Global (MG_WINDOW, ON);
    Set_Global (MG_QUIT, ON);
#ifndef  SCS_MODS
#else	/* SCS_MODS */
    
    if (Changes)
	if (curses_yes_no (1, "Save these changes in configuration file ?"))
	    get_mem(1);  /* save this custom version in the dig_cfg file */
#endif /* SCS */
    return (Pass);
}

/* display the current Customize settings */
Custom_settings (map)
    struct Map_info *map;
{
    char tmpstr[512];


    _Base_string (3, 36, " Current:");

    /*
    map_to_dig_thresh (map->prune_thresh);
    sprintf (tmpstr, "   %7.4lf", head.digit_thresh);
    */
    sprintf (tmpstr, "   %7.4lf", 2.0 * _map_to_dig_thresh (map->prune_thresh));
    _Base_string (4, 36, tmpstr);	/* digitizing thresh */

    /*
    map_to_dig_thresh (map->snap_thresh);
    sprintf (tmpstr, "   %7.4lf", head.digit_thresh);
    */
    sprintf (tmpstr, "   %7.4lf", _map_to_dig_thresh (map->snap_thresh));
    _Base_string (5, 36, tmpstr);	/* snapping thresh */

    _Base_string (7, 36, ON_OFF (Beep_On));
    /*
    _Base_string (8, 36, ON_OFF (Terse_On));
    _Base_string (8, 36, ON_OFF (Compress_File));
    */
    _Base_string (8, 36, ON_OFF (Auto_Window));
    _Base_string (9, 36,  Window_Device ?  "     MOUSE" : " DIGITIZER");
    _Base_string (10, 36,  Point_Device ?  "     MOUSE" : " DIGITIZER");
    _Base_string (11, 36,  Digtiz_Device ? "     MOUSE" : " DIGITIZER");
    sprintf (tmpstr, "%-10s", N_overlay);
    _Base_string (12, 36, tmpstr);
    sprintf (tmpstr, "%-10s", N_backdrop);
    _Base_string (13, 36, tmpstr);
    _Base_string (14, 36,  Remove_Draw ?   "    REMOVE" : "    REDRAW");
}
