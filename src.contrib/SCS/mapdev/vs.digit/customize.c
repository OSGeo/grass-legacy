/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
/*  Last modified by R.L. Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/

#include "digit.h"
#include "dig_head.h"
#include "popup.h"

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
    int chr, first=1;
    int custom=1, background_color, text_color, div_color;
    int menu_left, menu_top;
    int help_for, help_ret, help_cnt;
    int Changes= 0;
    static char *sav_opts[100];
    char buff[60];

    menu_left = Next_l + 1;
    menu_top = Next_t;
    Pass = 0;

    while(custom) 
    {
    background_color = D_translate_color(BC_MAIN) ;
    text_color       = D_translate_color(TC_MAIN) ;
    div_color        = D_translate_color(DC_MAIN) ;
    Dchoose(MEN.name) ;

    
    options[0] = "  Customize Menu\0";
    options[1] = "";
    sprintf(buff,"Set digitizing threshold %7.4lf",
                    2.0 * _map_to_dig_thresh (CM->prune_thresh));
    options[2] = (char *) malloc (strlen (buff) + 1);
    sprintf(options[2],"%s", buff);
    sprintf(buff,"Set snapping threshold %7.4lf",
                    _map_to_dig_thresh (CM->snap_thresh)); 
    options[3] = (char *) malloc (strlen (buff) + 1);
    sprintf(options[3],"%s", buff);
    if (Beep_On) options[4] = "BEEP > ON";
    else options[4] = "BEEP >OFF";
/*
    if (Auto_Window) options[5] = "Autowindow > ON";
    else options[5] = "Autowindow >OFF";
*/
    if (Auto_Number) options[5] = "Autonumber > ON";
    else options[5] = "Autonumber >OFF";
    options[6] = "Select a Raster Map";
    options[7] = "Select an Overlay Map";
    options[8] = "Display Options Menu";
    options[9] = "Display Color Options";
    options[10] = "";
    options[11] = "Help";
    options[12] = "Return to Main menu";
    options[13] = '\0' ;

            ret = popup_menu(
                    background_color,
                    text_color,
                    div_color,
                    menu_top,
                    menu_left,
                    MEN_SIZE,
		    "custom",
		    _custom
                    ) ;
    if (_custom) _custom = 0;  /* don't save menu panel more than once */

	    switch (ret) {
		case 2:
		    reset_thresh (CM);
		    Vect__write_head_binary (CM, &(CM->head));
		    Changes = 1;
		    break;
		case 3:
		    reset_snap_thresh (CM);
		    Changes = 1;
		    break;
		case 4:
		    TOGGLE (Beep_On);
		    Changes = 1;
		    break;
/*
		case 5:
		    TOGGLE (Auto_Window);
		    Changes = 1;
		    break;
*/
		case 5:
		    TOGGLE (Auto_Number);
		    Changes = 1;
		    break;
		case 6:
		    {
			register int ret;
			ret = ask_backdrop ();
			if (Disp_backdrop)
			  {
/*			  window_rout (U_north, U_south, U_east, U_west); */
	                  window_rout (CM->head.N, CM->head.S, CM->head.E, CM->head.W) ;
			  display_backdrop ();
			  }
		    }
		    break;
		case 7:
		    {
			register int ret;
			ret = ask_overlay ();
			if (Disp_overlay) display_overlay ();
		    }
		    break;
		case 8:
		    Changes = window_setup (CM);
		    erase_popup("display");
		    break;
		case 9:
		    Changes = color_setup (CM);
		    erase_popup("color");
		    break;
	        case 11:
		    for (help_cnt=0; help_cnt<=13; help_cnt++)
			  sav_opts[help_cnt] = options[help_cnt];
	            if (Help (&M_custom))
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
                                       menu_top,
                                       menu_left,
                                       MEN_SIZE,
				       "c_help",
				       first
                                       ) ;
                            if (first) first = 0;

	                    switch (help_ret) {
	                          case 2:
	                          case 3:
	                          case 4:
/*	                          case 5:     autowindow */
	                          case 5:
	                          case 6:
	                          case 7:
	                          case 8:
	                          case 9:
                                       Help_item(&M_custom,help_ret);
	                               break;
	                          case 11:
                                       Help_item(&M_main,help_ret);
	                               break;
	                          case 12:
	                               help_for = 0;
		                       options[0] = "  Customize Menu\0";
                                       background_color = D_translate_color(BC_MAIN) ;
                                       text_color       = D_translate_color(TC_MAIN) ;
                                       div_color        = D_translate_color(DC_MAIN) ;
				       erase_popup("c_help");
                                       erase_popup("info_help");
	                               break;
	                          default:
	                               break;
		                    }
                          }
                       }
	               break;
		case 12:
		    Pass = custom = 0;;
		    break;
/*
		case xx:
		    TOGGLE (Compress_File);
		    break;
*/
		default:
		    break;
	}
    }
    
    if (Changes)
    {
    options[0] = "Save configuration changes ?";
    options[1] = "Yes";
    options[2] = "No";
    options[3] = '\0' ;
    background_color = D_translate_color(BC_STOP) ;
    text_color       = D_translate_color(TC_STOP) ;
    div_color        = D_translate_color(DC_STOP) ;
    custom = first = 1;
    Dchoose(MEN.name);

    while(custom)
       {
            ret = popup_menu(
                    background_color,
                    text_color,
                    div_color,
                    Next_t + 8,
                    menu_left,
                    MEN_SIZE,
		    "change",
		    first
                    ) ;

	    if (first) first = 0;
	    switch (ret) {
	            case 1: 
			/* save this custom version in the vsdig_cfg file */
	               get_mem(1);  
		       custom = 0;
	               break;
	            case 2:
	               custom = 0;
	               break;
	           default:
	               break;
		    }
       erase_popup("change");
       }
    }
    return (Pass);
}

