/*
**  Written by Dave Gerdes  4/1988
**  US Army Construction Engineering Research Lab
*/
/*  Last modified by R.L. Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/


/*
**  Toolbox menu 
**
**  driver routine for toolbox menu
**
*/
#include "gis.h"
#include "digit.h"
#include "dig_head.h"
#include "popup.h"

int
Toolbox ()
{
    int command;		/* command user enters */
    int ret;			/* get return values from subrs */
    int Pass;			/* Holds value to return to caller */
    int chr;
    int stat;	/* for wait() */
    int tool=1, background_color, text_color, div_color;
    int menu_left, menu_top;
    int help_for, help_ret, help_cnt;
    static char *sav_opts[100];
    char buff[60];

    menu_left = Next_l + 1;
    menu_top = Next_t;

    while(tool) 
    {
    background_color = D_translate_color(BC_MAIN) ;
    text_color       = D_translate_color(TC_MAIN) ;
    div_color        = D_translate_color(DC_MAIN) ;

    Pass = 0;
    options[0] = "  Toolbox Menu";
    options[1] = "";
    options[2] = "Write out session";
    options[3] = "Display Unlabeled Areas";
    options[4] = "Display Open area lines";
    options[5] = "Display Duplicate lines";
    options[6] = "Display Node lines";
    options[7] = "Display Islands";
    options[8] = "";
    options[9] = "Replot Screen",
    options[10] = "Zoom";
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
		    "tool",
		    _tool
                    ) ;
    if (_tool) _tool = 0;     /* don't save menu panel more than once */

	    switch (ret) {
		case 2:
		    if ( 0 > write_out (1))
		    {
                        sprintf (buff, "Write FAILED!!! .");
		    }
		    else
		    {
			Changes_Made = 0;
                        sprintf (buff, "File updated...");
		    }
                    message[0] = (char *) malloc (strlen (buff) + 1);
                    sprintf(message[0],"%s", buff);
                    message[1] = " ";
                    message[2] = '\0';

                    Dchoose(MEN.name) ;
		    if (!Changes_Made)
                       {
		       popup_messg( "info", 1) ;
	               sleep(2);
                       erase_popup("info");
		       }
		    else
                       {
		       popup_messg( "warning", 1) ;
	               sleep(2);
                       erase_popup("warning");
		       }
		    break;
/*
#ifdef CERL
		case MTC_SHELL:
		    suspend ();
		    R_close_driver ();
		    ** swap_re_uids ();	    **
		    if (fork () == 0)
		    {
			** reset back to REAL user **
			if (0 > set_uid_to_user ())
			    _exit (127);
                        sprintf (buff,"  Type 'exit' to return .");
                        message[0] = (char *) malloc (strlen (buff) + 1);
                        sprintf(message[0],"%s", buff);
                        message[1] = " ";
                        message[2] = '\0';

                        Dchoose(MEN.name) ;
                        popup_messg( "warning", 1) ;
	                sleep(2);
                        erase_popup("warning");
			execl ("/bin/csh", "csh", 0);
			_exit (127);
		    }
		    else
		    {
			wait (&stat);
		    }
		    R_open_driver ();
		    ** swap_re_uids ();	    **
		    respend ();
#endif
*/
		    break;
		case 3:
                    Dchoose(DIG.name) ;
		    display_unlabeled_areas (CM);
                    Dchoose(MEN.name) ;
		    break;
		case 4:
                    Dchoose(DIG.name) ;
		    unfinished_areas (CM);
                    Dchoose(MEN.name) ;
		    break;
		case 5:
                    Dchoose(DIG.name) ;
		    display_duplicate_lines (CM);
                    Dchoose(MEN.name) ;
		    break;
		case 6:
                    Dchoose(DIG.name) ;
		    node_lines (CM);
                    Dchoose(MEN.name) ;
		    break;
		case 7:
                    Dchoose(DIG.name) ;
		    display_islands (CM);
                    Dchoose(MEN.name) ;
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
	               if (Help (&M_tool))
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
				       "t_help",
				       1
                                       ) ;
	                    switch (help_ret) {
	                          case 2:
	                          case 3:
	                          case 4:
	                          case 5:
	                          case 6:
	                          case 7:
                                       Help_item(&M_tool,help_ret);
	                               break;
	                          case 9:
	                          case 10:
	                          case 11:
                                       Help_item(&M_main,help_ret);
	                               break;
	                          case 12:
	                               help_for = 0;
		                       options[0] = "  Toolbox Menu\0";
                                       background_color = D_translate_color(BC_MAIN) ;
                                       text_color       = D_translate_color(TC_MAIN) ;
                                       div_color        = D_translate_color(DC_MAIN) ;
				       erase_popup("t_help");
                                       erase_popup("info_help");
	                               break;
	                          default:
	                               break;
		                    }
                          }
                       }
	               break;
		case 12:
		    Pass = tool = 0;
		    break;
		default:
		    break;
	}
    }
    return (Pass);
}

suspend ()
{
    Old_tty ();
}

respend ()
{
    New_tty();
}

