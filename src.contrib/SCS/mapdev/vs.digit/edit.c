/*
**  Written by Mike Higgins 
**    Last modified by Dave Gerdes  4/1988
**  US Army Construction Engineering Research Lab
*/
/*  Last modified by R.L. Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/


#include "digit.h"
#include "dig_head.h"
#include "popup.h"
#include <stdio.h>

/*
**  Edit menu 
**
**  driver routine for Edit menu
**
*/
int
Edit()
{
    int command;		/* command user enters */
    int ret;			/* get return values from subrs */
    int Pass;			/* Holds value to return to caller */
    int chr;
    int edit=1, background_color, text_color, div_color;
    int menu_left, menu_top;
    int help_for, help_ret, help_cnt;
    static char *sav_opts[100];
    char buff[60];

    menu_left = Next_l + 1;
    menu_top = Next_t;

    while(edit)
    {
    options[0] = "   Edit Menu";
    options[1] = "";
    options[2] = "Remove line";
    options[3] = "Remove site";
    options[4] = "Snap line to node";
    options[5] = "Snap line to line";
    options[6] = "Break a line";
    options[7] = "Move a point";
    options[8] = "Move a line or site";
    options[9] = "Re-type a line (AREA/LINE)";
    options[10] = "Display node in threshold";
    options[11] = "Remove BLOCK of lines";
    options[12] = "";
    options[13] = "Replot Screen";
    options[14] = "Zoom";
    options[15] = "Help";
    options[16] = "Return to Main menu";
    options[17] = '\0';


    background_color = D_translate_color(BC_MAIN) ;
    text_color       = D_translate_color(TC_MAIN) ;
    div_color        = D_translate_color(DC_MAIN) ;

    ret = popup_menu(
                    background_color,
                    text_color,
                    div_color,
                    menu_top,
                    menu_left,
                    MEN_SIZE,
		    "edit",
		    _edit
                    ) ;
    if (_edit) _edit = 0;     /* don't save menu panel more than once */

	    switch (ret) {
	            case 2:
                        Dchoose(DIG.name) ;
		        remove_line (CM, LINE | AREA);
                        Dchoose(MEN.name) ;
		        break;
	            case 3:
                        Dchoose(DIG.name) ;
		        remove_line (CM, DOT);
                        Dchoose(MEN.name) ;
		        break;
	            case 4:
                        Dchoose(DIG.name) ;
		        snap_nodes(CM);
                        Dchoose(MEN.name) ;
		        break;
                    case 5:
                        Dchoose(DIG.name) ;
                        intersect_line (CM);
                        Dchoose(MEN.name) ;
                        break;
	            case 6:
                        Dchoose(DIG.name) ;
		        break_line (CM);
                        Dchoose(MEN.name) ;
		        break;
	            case 7:
                        Dchoose(DIG.name) ;
		        move_point (CM);
                        Dchoose(MEN.name) ;
		        break;
	            case 8:
                        Dchoose(DIG.name) ;
		        move_line (CM);
                        Dchoose(MEN.name) ;
		        break;
	            case 9:
                        Dchoose(DIG.name) ;
		        retype_line (CM);
                        Dchoose(MEN.name) ;
		        break;
	            case 10:
                        Dchoose(DIG.name) ;
		        within_a_thresh (CM);
                        Dchoose(MEN.name) ;
		        break;
	            case 11:
                        Dchoose(DIG.name) ;
		        remove_block (CM);
                        Dchoose(MEN.name) ;
		        break;
	            case 13:
                        Dchoose(DIG.name) ;
	                clear_window ();
	                replot (CM);
                        Dchoose(MEN.name) ;
	                break;
	            case 14:
                        Dchoose(MEN.name) ;
	                zoom_window ();
		        erase_popup("zoom");
		        _zoom = 1;
	                break;
	            case 15:
		        for (help_cnt=0; help_cnt<=17; help_cnt++)
			  sav_opts[help_cnt] = options[help_cnt];
	                if (Help (&M_edit))
		        {
                        background_color = D_translate_color(BC_HELP) ;
                        text_color       = D_translate_color(TC_HELP) ;
                        div_color        = D_translate_color(DC_HELP) ;
		        help_for = 1;
                        while(help_for)
                          {
			  for (help_cnt=0; help_cnt<=17; help_cnt++)
			    options[help_cnt] = sav_opts[help_cnt];
		           options[0] = "   HELP for",
                           options[16] = "Exit from Help menu";
                               help_ret =  popup_menu(
                                       background_color,
                                       text_color,
                                       div_color,
                                       menu_top,
                                       menu_left,
                                       MEN_SIZE,
				       "e_help",
				       1
                                       ) ;
	                    switch (help_ret) {
	                          case 2:
	                          case 3:
	                          case 4:
	                          case 5:
	                          case 6:
	                          case 7:
	                          case 8:
	                          case 9:
	                          case 10:
	                          case 11:
                                       Help_item(&M_edit,help_ret);
	                               break;
	                          case 13:
	                          case 14:
	                          case 15:
                                       Help_item(&M_main,help_ret-4);
	                               break;
	                          case 16:
	                               help_for = 0;
		                       options[0] = "   Edit Menu";
                                       background_color = D_translate_color(BC_MAIN) ;
                                       text_color       = D_translate_color(TC_MAIN) ;
                                       div_color        = D_translate_color(DC_MAIN) ;
				       erase_popup("e_help");
                                       erase_popup("info_help");
	                               break;
	                          default:
	                               break;
		                    }
                          }
                        }
	                break;
	            case 16:
		        edit = 0;
		        break;
	            default:
		        break;	 /* should not get here */
	            }
    }
    
    G_clear_screen ();
    return (Pass);
}
