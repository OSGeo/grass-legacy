/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
** modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/

#include "digit.h"
#include "dig_head.h"
#include "popup.h"
#include "gis.h"
#include <stdio.h>

interact()
{
    int command, background_color, text_color, menu_left;
    int div_color, ret, chr;
    int help_for, help_ret, help_cnt;
    static char *sav_opts[100];
    char buff[60];

    for(;;)
    {
    options[0] = "   MAIN MENU";
    options[1] = "";
    options[2] = "Digitize";
    options[3] = "Edit";
    options[4] = "Label";
    options[5] = "Customize";
    options[6] = "Toolbox";
    options[7] = "Window";
    options[8] = "";
    options[9] = "Replot Screen";
    options[10] = "Zoom";
    options[11] = "Help";
    options[12] = "Quit";
    options[13] = '\0';


    background_color = D_translate_color(BC_MAIN) ;
    text_color       = D_translate_color(TC_MAIN) ;
    div_color        = D_translate_color(DC_MAIN) ;
    menu_left = (int)((D_east * 100.) / (double)screen_right);
    Dchoose(MEN.name) ;

    ret = popup_menu(
                    background_color,
                    text_color,
                    div_color,
                    MEN_TOP,
                    menu_left,
                    MEN_SIZE,
		    "main",
		    _main
                    ) ;
    if (_main) _main = 0;     /* don't save menu panel more than once */

    switch (ret) {
	            case 2:
		       Digitize(CM);
		       erase_popup("digit");
		       _digit = 1;
	               break;
	            case 3:
		       Edit(CM);
		       erase_popup("edit");
		       _edit = 1;
	               break;
	            case 4:
		       Label (CM);
		       erase_popup("label");
		       _label = 1;
	               break;
	            case 5:
	               Customize ();
		       erase_popup("custom");
		       _custom = 1;
	               break;
	            case 6:
	               Toolbox ();
		       erase_popup("tool");
		       _tool = 1;
	               break;
	            case 7:
	               Window ();
		       erase_popup("window");
		       _wind = 1;
	               break;
	            case 9:
                       Dchoose(DIG.name) ;
	               clear_window ();
	               replot (CM);
                       Dchoose(MEN.name) ;
	               break;
	            case 10:
                       Dchoose(MEN.name) ;
	               zoom_window ();
		       erase_popup("zoom");
		       _zoom = 1;
	               break;
	            case 11:
		       for (help_cnt=0; help_cnt<=13; help_cnt++)
			  sav_opts[help_cnt] = options[help_cnt];
	               if (Help (&M_main))
		       {
                       background_color = D_translate_color(BC_HELP) ;
                       text_color       = D_translate_color(TC_HELP) ;
                       div_color        = D_translate_color(DC_HELP) ;
		       help_for = 1;
                       while(help_for)
                          {
		          for (help_cnt=0; help_cnt<=13; help_cnt++)
			    options[help_cnt] = sav_opts[help_cnt];
		          options[0] = "   HELP for",
                          options[12] = "Exit from Help menu";
                               help_ret =  popup_menu(
                                       background_color,
                                       text_color,
                                       div_color,
                                       MEN_TOP,
                                       menu_left,
                                       MEN_SIZE,
				       "m_help",
				       1
                                       ) ;
	                    switch (help_ret) {
	                          case 2:
	                          case 3:
	                          case 4:
	                          case 5:
	                          case 6:
	                          case 7:
	                          case 9:
	                          case 10:
	                          case 11:
                                       Help_item(&M_main,help_ret);
	                               break;
	                          case 12:
	                               help_for = 0;
		                       options[0] = "   MAIN MENU";
                                       background_color = D_translate_color(BC_MAIN) ;
                                       text_color       = D_translate_color(TC_MAIN) ;
                                       div_color        = D_translate_color(DC_MAIN) ;
				       erase_popup("m_help");
                                       erase_popup("info_help");
	                               break;
	                          default:
	                               break;
		                    }
                          }
                       }
	               break;
	            case 12:
	               if (quit_it()) return ;
	               break;
	           default:
	               break;
		    }
    }
}

