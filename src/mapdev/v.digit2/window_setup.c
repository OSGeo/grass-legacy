/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"

/* returns 0 if no changes  or 1 if changes made */
window_setup (map)
    struct Map_info *map;
{
    int command;		/* command user enters */
    int ret;			/* get return values from subrs */
    int Pass;			/* Holds value to return to caller */
    int chr;
    int changed;

    Pass = 0;
    Set_Global (MG_DIGIT, OFF);
    Set_Global (MG_EDIT, OFF);
    Set_Global (MG_LABEL, OFF);
    Set_Global (MG_CUSTOM, OFF);
    Set_Global (MG_TOOL, OFF);
    Set_Global (MG_WINDOW, OFF);
    Set_Global (MG_QUIT, OFF);

    changed = 0;
    while(1) 
    {
	_Clear_info ();
	_update_global_menu ();
	_Write_generic_win(&M_display);
	Display_settings (&M_display);
	_Base_refresh ();
	_Info_refresh ();

	if ((command = get_menu_command (&M_display, &chr)) > 0)
	{
	    if (command == MSC_QUIT)
	    {
		Pass = 0;
		goto DISP_END;
	    }
	    changed = 1;
	    switch(command) {
		case MSC_OVERLAY:
		    TOGGLE (Disp_overlay);
		    break;
		case MSC_LINES:
		    TOGGLE (Disp_lines);
		    break;
		case MSC_SITES:
		    TOGGLE (Disp_sites);
		    break;
		case MSC_SLABELS:
		    TOGGLE (Disp_slabels);
		    break;
		case MSC_POINTS:
		    TOGGLE (Disp_points);
		    break;
		case MSC_NODES:
		    TOGGLE (Disp_nodes);
		    break;
		case MSC_LABELS:
		    TOGGLE (Disp_labels);
		    break;
		case MSC_OUTLINE:
		    TOGGLE (Disp_outline);
		    break;
		case MSC_MARKERS:
		    TOGGLE (Disp_markers);
		    break;
		case MSC_LLINES:
		    TOGGLE (Disp_llines);
		    break;
		case MSC_LLABELS:
		    TOGGLE (Disp_llabels);
		    break;
		case MSC_RESET:
		    set_default_display();
		    break;
		case MSC_NOCHANGE:
		    changed = 0;
		    break;
		default:
		    break;
	    }
	}
	else
	{
	    if ((ret = global_menu (chr, &M_display)) > 0)
	    {
		Pass = ret;	
		break;  /* return and execute new command */
	    }
	    if (ret < 0)
		BEEP;
	}
    }
DISP_END:

    if (changed)
    {
	R_standard_color (dcolors[CLR_ERASE]);
	D_erase_window();
	outline_window();
	replot (map);
    }

    Set_Global (MG_DIGIT, ON);
    Set_Global (MG_EDIT, ON);
    Set_Global (MG_LABEL, ON);
    Set_Global (MG_CUSTOM, ON);
    Set_Global (MG_TOOL, ON);
    Set_Global (MG_WINDOW, ON);
    Set_Global (MG_QUIT, ON);

    return (changed);
}

/* display the current Customize settings */
Display_settings (menu)
    struct Menu_head *menu;
{
    char tmpstr[512];


    _Base_string (3, 27, " Current:");
    _Base_string (4, 27, ON_OFF (Disp_labels));
    _Base_string (5, 27, ON_OFF (Disp_llabels));
    _Base_string (6, 27, ON_OFF (Disp_slabels));
    _Base_string (7, 27, ON_OFF (Disp_markers));
    _Base_string (8, 27, ON_OFF (Disp_outline));
    _Base_string (9, 27, ON_OFF (Disp_llines));
    _Base_string (10, 27, ON_OFF (Disp_lines));
    _Base_string (11, 27, ON_OFF (Disp_sites));
    _Base_string (12, 27, ON_OFF (Disp_nodes));
    _Base_string (13, 27, ON_OFF (Disp_points));
    /*
    _Base_string (12, 27, ON_OFF (Disp_overlay));
    */
}
