/*  @(#)mouse_yn.c	1.1  6/26/87  */
/* modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/

#include <stdio.h>
#include "digit.h"
#include "popup.h"

mouse_yes_no ( header)
	char	*header ;
{
	int cnt, button ;
	int	screen_x, screen_y ;
        int menu_left, menu_top;
        int ret, chr;
	char *ptr1, *ptr2;

        menu_left = Next_l + 1;
        menu_top = Next_t;

	screen_x = screen_y = 1;

	    /* search the header string for \n (\012), mult lines */
        ptr1 = ptr2 = header;
	button = 0;
	while (*ptr1 != '\0')
	     {
	     if (*ptr1 == '\012') button++;
	     ptr1++;
	     }

        if (button > 0)
	   {
	   ptr1 = header;
	   for (cnt=0; cnt<=button; cnt++)
	      {
	      while (*ptr1 != '\012')
		{
		ptr1++;
		if (*ptr1 == '\0') break;
		}
	      if (*ptr1 == '\012') 
		{
		*(ptr1) = '\0'; 
		ptr1++;
		}
              buttons[cnt] = (char *) malloc (strlen (ptr2) + 1);
              sprintf(buttons[cnt],"%s", ptr2);
	      ptr2 = ptr1;
	      }
	   }
        else
	   {
           buttons[0] = (char *) malloc (strlen (header) + 1);
           sprintf(buttons[0],"%s", header);
           cnt = 1;
	   }
        buttons[cnt] = "Buttons:";
        buttons[cnt+1] = "Left:   yes";
        buttons[cnt+2] = "Middle: no";
        buttons[cnt+3] = "Right:  yes";
        buttons[cnt+4] = "  ";
        buttons[cnt+5] = '\0';

        Dchoose(MEN.name) ;
        popup_butns( menu_top, menu_left, buttons, "yes_no", 1) ;

	R_get_location_with_pointer ( &screen_x, &screen_y, &button) ;

	erase_popup("yes_no");
	return (!(button == 2)) ;
}

/* this is for node_lines () */
mouse_next_prev ( header)
	char	*header ;
{
	int button ;
	int	screen_x, screen_y ;
        int menu_left, menu_top;
        int ret, chr;

        menu_left = Next_l + 1;
        menu_top = Next_t + 9;

        buttons[0] = (char *) malloc (strlen (header) + 1);
        sprintf(buttons[0],"%s\0", header);
        buttons[1] = "Buttons:";
        buttons[2] = "Left:   Previous line";
        buttons[3] = "Middle: Quit";
        buttons[4] = "Right:  Next line";
        buttons[5] = "  ";
        buttons[6] = '\0';

        Dchoose(MEN.name) ;
        popup_butns( menu_top, menu_left, buttons, "mous_n_p", 1) ;
        Dchoose(DIG.name) ;

	R_get_location_with_pointer ( &screen_x, &screen_y, &button) ;

	erase_popup("mous_n_p");
	return(button) ;
}

