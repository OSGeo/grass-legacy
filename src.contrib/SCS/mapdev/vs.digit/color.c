/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
/*  Last modified by R.L. Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/

#include "digit.h"
#include "popup.h"

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
    int chr, first=1;
    int changed;
    int setup=1, background_color, text_color, div_color;
    int menu_left, menu_top;
    char buf[80];

    menu_left = Next_l + 1;
    menu_top = Next_t;
    Pass = 0;
    changed = 0;

    while(setup) 
    {
    background_color = D_translate_color(BC_MAIN) ;
    text_color       = D_translate_color(TC_MAIN) ;
    div_color        = D_translate_color(DC_MAIN) ;

    options[0] = "  Color Menu\0";
    options[1] = "";
    sprintf(buf, "Areas                    >%8s",color_name(CLR_AREA));
    options[2] = (char *) malloc (strlen (buf) + 1);
    sprintf(options[2],"%s", buf);
    sprintf(buf, "Lines                    >%8s",color_name(CLR_LINE));
    options[3] = (char *) malloc (strlen (buf) + 1);
    sprintf(options[3],"%s", buf);
    sprintf(buf, "Sites                    >%8s",color_name(CLR_SITE));
    options[4] = (char *) malloc (strlen (buf) + 1);
    sprintf(options[4],"%s", buf);
    sprintf(buf, "Labeled areas            >%8s",color_name(CLR_LAREA));
    options[5] = (char *) malloc (strlen (buf) + 1);
    sprintf(options[5],"%s", buf);
    sprintf(buf, "Labeled lines            >%8s",color_name(CLR_LLINE));
    options[6] = (char *) malloc (strlen (buf) + 1);
    sprintf(options[6],"%s", buf);
    sprintf(buf, "Labeled sites            >%8s",color_name(CLR_LSITE));
    options[7] = (char *) malloc (strlen (buf) + 1);
    sprintf(options[7],"%s", buf);
    sprintf(buf, "Nodes w/ 1 line          >%8s",color_name(CLR_1_NODE));
    options[8] = (char *) malloc (strlen (buf) + 1);
    sprintf(options[8],"%s", buf);
    sprintf(buf, "Nodes w/ 2 or more lines >%8s",color_name(CLR_2_NODE));
    options[9] = (char *) malloc (strlen (buf) + 1);
    sprintf(options[9],"%s", buf);
    sprintf(buf, "Highlight                >%8s",color_name(CLR_HIGHLIGHT));
    options[10] = (char *) malloc (strlen (buf) + 1);
    sprintf(options[10],"%s", buf);
    sprintf(buf, "Overlay map              >%8s",color_name(CLR_OVERLAY));
    options[11] = (char *) malloc (strlen (buf) + 1);
    sprintf(options[11],"%s", buf);
    sprintf(buf, "Bar Scale                >%8s",color_name(CLR_SCALE));
    options[12] = (char *) malloc (strlen (buf) + 1);
    sprintf(options[12],"%s", buf);
    options[13] = "Reset Defaults";
    options[14] = "";
    options[15] = "Quit";
    options[16] = '\0' ;

        ret = popup_menu(
                    background_color,
                    text_color,
                    div_color,
                    menu_top,
                    menu_left,
                    MEN_SIZE,
		    "color",
		    first
                    ) ;

       if (first) first = 0;

	    switch (ret) {
		case 2:
		    CLR_AREA = color_toggle (CLR_AREA);
                    changed = 1;
		    break;
		case 3:
		    CLR_LINE = color_toggle (CLR_LINE);
                    changed = 1;
		    break;
		case 4:
		    CLR_SITE = color_toggle (CLR_SITE);
                    changed = 1;
		    break;
		case 5:
		    CLR_LAREA = color_toggle (CLR_LAREA);
		    CLR_AMARK = CLR_LAREA;
		    CLR_ALABEL = CLR_LAREA;
                    changed = 1;
		    break;
		case 6:
		    CLR_LLINE = color_toggle (CLR_LLINE);
		    CLR_LLABEL = CLR_LLINE;
                    changed = 1;
		    break;
		case 7:
		    CLR_LSITE = color_toggle (CLR_LSITE);
                    changed = 1;
		    break;
		case 8:
		    CLR_1_NODE = color_toggle (CLR_1_NODE);
                    changed = 1;
		    break;
		case 9:
		    CLR_2_NODE = color_toggle (CLR_2_NODE);
                    changed = 1;
		    break;
		case 10:
		    CLR_HIGHLIGHT = color_toggle (CLR_HIGHLIGHT);
                    changed = 1;
		    break;
		case 11:
		    CLR_OVERLAY = color_toggle (CLR_OVERLAY);
		    CLR_UNKNOWN = CLR_OVERLAY;
                    changed = 1;
		    break;
		case 12:
		    CLR_SCALE = color_toggle (CLR_SCALE);
                    changed = 1;
		    break;
		case 13:
		    init_colors ();
                    changed = 1;
		    break;
		case 15:
		    setup = 0;
		    break;
		default:
		    break;
	}
    }

    if (changed)
    {
	outline_window();
        clear_window ();
	replot (map);
    }
    return (changed);
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
