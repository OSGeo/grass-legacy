/*  @(#)leave.c	2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
**  Re-Written by Ron Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/

#include "popup.h"
#include "digit.h"

leave()
{
    int background_color, text_color, menu_left;
    int div_color, ret, chr;
    
    options[0] = "    Continue ??";
    options[1] = "Yes";
    options[2] = "No";
    options[3] =  '\0';


    background_color = D_translate_color(BC_STOP) ;
    text_color       = D_translate_color(TC_STOP) ;
    div_color        = D_translate_color(DC_STOP) ;
    menu_left = (int)((D_east * 100.) / (double)screen_right);

    for(;;)
    {
            ret = popup_menu(
                    background_color,
                    text_color,
                    div_color,
                    MEN_TOP,
                    menu_left,
                    MEN_SIZE,
		    "continu",
		    0
                    ) ;

	    switch (ret) {
	            case 1: 
	               Dchoose(DIG.name) ;
		       return (1);
	               break;
	            case 2:
	               return (0) ;
	               break;
	           default:
	               break;
		    }
    }

}


ready()
{
    int background_color, text_color;
    int div_color, ret, chr;
    int menu_left, menu_top;

    options[0] = "Ready ??";
    options[1] = "Yes";
    options[2] = " ";
    options[3] = '\0';


    background_color = D_translate_color(BC_STOP) ;
    text_color       = D_translate_color(TC_STOP) ;
    div_color        = D_translate_color(DC_STOP) ;
    menu_left = Next_l - 2;
    menu_top = Next_t + 8;

    for(;;)
    {
            ret = popup_menu(
                    background_color,
                    text_color,
                    div_color,
                    menu_top,
                    menu_left,
                    MEN_SIZE,
		    "ready",
		    1
                    ) ;

	    switch (ret) {
	            case 1: 
		       erase_popup("ready");
	               return ;
	               break;
	           default:
	               break;
		    }
    }
}

quit_it()
{
    int background_color, text_color;
    int div_color, ret, chr;
    char buff[60];

    options[0] = "    Leave Digit";
    options[1] = "Yes";
    options[2] = "No";
    options[3] = '\0';

    background_color = D_translate_color(BC_STOP) ;
    text_color       = D_translate_color(TC_STOP) ;
    div_color        = D_translate_color(DC_STOP) ;

    for(;;)
    {
            ret = popup_menu(
                    background_color,
                    text_color,
                    div_color,
                    Next_t + 8,
                    Next_l - 2,
                    MEN_SIZE,
		    "quit",
		    1
                    ) ;

	    switch (ret) {
	            case 1: 
		       erase_popup("quit");
		       erase_popup("main");
/*                     R_standard_color (dcolors[CLR_ERASE]);
                       D_erase_window ();*/
		       return (1);
	               break;
	            case 2:
		       erase_popup("quit");
	               return (0) ;
	               break;
	           default:
	               break;
		    }
    }
}
