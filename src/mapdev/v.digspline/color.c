/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"

static char *color_names[] = {
    "        ",
    "   white",
    "   black",
    "  yellow",
    "    blue",
    "     red",
    "   green",
    "  orange",
    "    grey",
    " magenta",
    "    aqua",
    "  indigo",
    "  violet",
    "   brown",
    "        ",
    "        "
};


/* returns 0 no change, or 1 changes made */
color_setup (map)
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
	_Write_generic_win(&M_color);
	Display_color_settings ();
	_Base_refresh ();
	_Info_refresh ();

	if ((command = get_menu_command (&M_color, &chr)) > 0)
	{
	    if (command == MOC_QUIT)
	    {
		Pass = 0;
		goto COLR_END;
	    }
	    changed = 1;
	    switch(command) {
    /*
		case MOC_ALABEL:
		    CLR_ALABEL = color_toggle (CLR_ALABEL);
		    break;
		case MOC_LLABEL:
		    CLR_LLABEL = color_toggle (CLR_LLABEL);
		    break;
    */
		case MOC_LLINE:
		    CLR_LLINE = color_toggle (CLR_LLINE);
		    CLR_LLABEL = CLR_LLINE;
		    break;
		case MOC_LSITE:
		    CLR_LSITE = color_toggle (CLR_LSITE);
		    break;
		case MOC_SITE:
		    CLR_SITE = color_toggle (CLR_SITE);
		    break;
		case MOC_LAREA:
		    CLR_LAREA = color_toggle (CLR_LAREA);
		    CLR_AMARK = CLR_LAREA;
		    CLR_ALABEL = CLR_LAREA;
		    break;
		case MOC_AREA:
		    CLR_AREA = color_toggle (CLR_AREA);
		    break;
		case MOC_LINE:
		    CLR_LINE = color_toggle (CLR_LINE);
		    break;
/*
		case MOC_0NODE:
		    CLR_0_NODE = color_toggle (CLR_0_NODE);
		    break;
*/
		case MOC_1NODE:
		    CLR_1_NODE = color_toggle (CLR_1_NODE);
		    break;
		case MOC_2NODE:
		    CLR_2_NODE = color_toggle (CLR_2_NODE);
		    break;
		case MOC_HIGHLIGHT:
		    CLR_HIGHLIGHT = color_toggle (CLR_HIGHLIGHT);
		    break;
		case MOC_BACKGROUND:
		    CLR_ERASE = color_toggle (CLR_ERASE);
		    break;
		case MOC_OVERLAY:
		    CLR_OVERLAY = color_toggle (CLR_OVERLAY);
		    CLR_UNKNOWN = CLR_OVERLAY;
		    break;
		case MOC_RESET:
		    init_colors ();
		    break;
		case MOC_NOCHANGE:
		    changed = 0;
		    break;
		case MOC_UPDATE:
		    R_standard_color (dcolors[CLR_ERASE]);
		    D_erase_window();
		    outline_window();
		    replot (map);
		    changed = 0;
		    break;
		default:
		    break;
	    }
	}
	else
	{
	    if ((ret = global_menu (chr, &M_color)) > 0)
	    {
		Pass = ret;	
		break;  /* return and execute new command */
	    }
	    if (ret < 0)
		BEEP;
	}
    }
COLR_END:

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
Display_color_settings ()
{
    _Base_string (3, 36, " Current:");
    _Base_string (4, 38, color_name (CLR_AREA));
    _Base_string (5, 38, color_name (CLR_LINE));
    _Base_string (6, 38, color_name (CLR_SITE));
    _Base_string (7, 38, color_name (CLR_LAREA));
    _Base_string (8, 38, color_name (CLR_LLINE));
    _Base_string (9, 38, color_name (CLR_LSITE));
    _Base_string (10, 38, color_name (CLR_1_NODE));
    _Base_string (11, 38, color_name (CLR_2_NODE));
    _Base_string (12, 38, color_name (CLR_HIGHLIGHT));
    _Base_string (13, 38, color_name (CLR_ERASE));
    _Base_string (14, 38, color_name (CLR_OVERLAY));
    /*
    _Base_string (9, 38, color_name (CLR_0_NODE));
    */
}

color_toggle (color)
    register int color;
{
    color++;
    if (color >= MAXCOLORS)
	color = 1;
    return (color);
}

char *
color_name (color)
    int color;
{
    if (color < 1 || color > 13)
	return ("none");
    return (color_names[color]);
}
