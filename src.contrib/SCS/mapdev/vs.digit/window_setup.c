/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
/*  Last modified by R.L. Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/

#include "digit.h"
#include "popup.h"

/* returns 0 if no changes  or 1 if changes made */
window_setup (map)
    struct Map_info *map;
{
    int command;		/* command user enters */
    int ret;			/* get return values from subrs */
    int Pass;			/* Holds value to return to caller */
    int chr, first=1;
    int changed;
    char mem_file[60], buff[100];
    int setup=1, background_color, text_color, div_color;
    int menu_left, menu_top;

    menu_left = Next_l + 1;
    menu_top = Next_t;
    Pass = 0;
    changed = 0;

    while(setup) 
    {
    background_color = D_translate_color(BC_MAIN) ;
    text_color       = D_translate_color(TC_MAIN) ;
    div_color        = D_translate_color(DC_MAIN) ;

    options[0] = "  Display Menu";
    options[1] = "";
    if (Disp_labels) options[2] = "Area Labels > ON";
    else options[2] = "Area Labels >OFF";
    if (Disp_llabels) options[3] = "Line Labels > ON";
    else options[3] = "Line Labels >OFF";
    if (Disp_slabels) options[4] = "Site Labels > ON";
    else options[4] = "Site Labels >OFF";
    if (Disp_names) options[5] = "Labels by NAME";
    else options[5] = "Labels by NUM ";
    if (Disp_markers) options[6] = "Area Markers > ON";
    else options[6] = "Area Markers >OFF";
    if (Disp_outline) options[7] = "Area Border lines > ON";
    else options[7] = "Area Border lines >OFF";
    if (Disp_llines) options[8] = "Labeled lines > ON";
    else options[8] = "Labeled lines >OFF";
    if (Disp_lines) options[9] = "Lines > ON";
    else options[9] = "Lines >OFF";
    if (Disp_sites) options[10] = "Sites > ON";
    else options[10] = "Sites >OFF";
    if (Disp_nodes) options[11] = "Nodes > ON";
    else options[11] = "Nodes >OFF";
    if (Disp_points) options[12] = "Points in lines > ON";
    else options[12] = "Points in lines >OFF";
    if (Disp_backdrop) options[13] = "Raster display > ON";
    else options[13] = "Raster display >OFF";
    if (Disp_overlay) options[14] = "Overlay display > ON";
    else options[14] = "Overlay display >OFF";
    options[15] = "Reset Defaults";
    options[16] = "";
    options[17] = "Quit";
    options[18] = '\0' ;

    ret = popup_menu(
                    background_color,
                    text_color,
                    div_color,
                    menu_top,
                    menu_left,
                    MEN_SIZE,
		    "display",
		    first
                    ) ;

	 if (first) first = 0;
	    switch (ret) {

		case 2:
		    TOGGLE (Disp_labels);
                    changed = 1;
		    break;
		case 3:
		    TOGGLE (Disp_llabels);
                    changed = 1;
		    break;
		case 4:
		    TOGGLE (Disp_slabels);
                    changed = 1;
		    break;
		case 5:
		    if (cats_changed)
		      {
                      sprintf(buff,"You have modified your labels\nDo you want to update the dig/cats file ? .");
	              if (mouse_yes_no (buff))
	                 {
                         sprintf(buff,"cp %s/SUBJ/%s %s",
		             N_path,N_subj_file, N_cats_file);
                         system(buff);
		         TOGGLE (Disp_names);
                         changed = 1;
			 cats_changed = 0;
			 }
		      }
		    else
		      {
		      TOGGLE (Disp_names);
                      changed = 1;
		      }
		    break;
		case 6:
		    TOGGLE (Disp_markers);
                    changed = 1;
		    break;
		case 7:
		    TOGGLE (Disp_outline);
                    changed = 1;
		    break;
		case 8:
		    TOGGLE (Disp_llines);
                    changed = 1;
		    break;
		case 9:
		    TOGGLE (Disp_lines);
                    changed = 1;
		    break;
		case 10:
		    TOGGLE (Disp_sites);
                    changed = 1;
		    break;
		case 11:
		    TOGGLE (Disp_nodes);
                    changed = 1;
		    break;
		case 12:
		    TOGGLE (Disp_points);
                    changed = 1;
		    break;
		case 13:
		    TOGGLE (Disp_backdrop);
		    break;
		case 14:
		    TOGGLE (Disp_overlay);
		    break;
		case 15:
		    set_default_display();
                    changed = 1;
		    break;
		case 17:
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

