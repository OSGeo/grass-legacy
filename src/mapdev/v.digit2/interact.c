/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "dig_head.h"
#include <stdio.h>

interact()
{
    int command;
    int ret;
    int chr;


    while(1) 
    {
	_Clear_info ();
	update_global_menu ();

	_Write_base_win();
	_Write_header_info();
	_Write_type_info();
	Main_info ();
	/* _Base_refresh (); */

	
	if ((command = get_menu_command (&M_main, &chr)) > 0)
	{
	    switch(command)
	    {
		/** this case for debugging  **/
		/*
		case 'x':
		    pr_nodes();
		    break;
		*/
		default:
		    break;
	    }
	}
	else
	{
	    ret = global_menu (chr, &M_main);
	    while (ret > 0)
	    {
		Set_G_Mask (MG_QUIT, OFF);
		switch (ret) {
		    case MGI_EDIT:
			ret = Edit(CM);
			break;
		    case MGI_DIGIT:
			ret = Digitize(CM);
			break;
		    case MGI_LABEL:
			ret = Label (CM);
			break;
		    case MGI_QUIT:
			return (0);
		    default:
			ret = 0;
			break;
		    }
		Set_G_Mask (MG_QUIT, ON);
	    }
	    if (ret < 0)
		BEEP;
	}
    }
}

Main_info ()
{
    Base_string (13, 21, Dig_Enabled   ? "Enabled  " : "Disabled ");
}
