/*
** $Id$
**  Written by Dave Gerdes  4/1988
**  US Army Construction Engineering Research Lab
*/


/*
**  Toolbox menu 
**
**  driver routine for toolbox menu
**
*/
#include <curses.h>
#include <unistd.h>
#include "raster.h"
#include "gis.h"
#include "digit.h"
#include "keyboard.h"
#include "dig_curses.h"
#include "Map_proto.h"
#include "local_proto.h"

int 
Toolbox (void)
{
    int command;		/* command user enters */
    int ret;			/* get return values from subrs */
    int Pass;			/* Holds value to return to caller */
    int chr;
    int stat;	/* for wait() */

    Pass = 0;
    Set_Global (MG_DIGIT, OFF);
    Set_Global (MG_EDIT, OFF);
    Set_Global (MG_LABEL, OFF);
    Set_Global (MG_CUSTOM, OFF);
    Set_Global (MG_TOOL, OFF);
    Set_Global (MG_QUIT, OFF);
    Set_Global (MG_WINDOW, ON);

    while(1) 
    {
	_Clear_info ();
	update_global_menu ();
	Write_generic_win(&M_tool);

	if ((command = get_menu_command (&M_tool, &chr)) > 0)
	{
	    switch(command) {
#ifdef FOO
		case MTC_CELL:
		    /* first write out file */
		    _Clear_info ();
		    if ( 0 > write_out (1))
		    {
			BEEP;
			Write_info (2, "Write FAILED!!!");
			sleep (4);
		    }
		    else
		    {
			Changes_Made = 0;
			Write_info (2, "File updated...");
			sleep (2);
		    }
		    mysuspend ();
		    fprintf (stderr, "\n Creating Cell File\n");
		    gorun ("vect.to.cell", N_name);
		    flush_keyboard ();
		    fprintf (stderr, "Press enter to continue. ");
		    {
			char buf[1024];
			gets (buf);
		    }
		    myrespend ();
		    break;
		case MTC_DCELL:
		    /* first write out file */
		    _Clear_info ();
		    mysuspend ();
		    R_close_driver ();
		    fprintf (stderr, "\n Displaying Cell File\n");

		    gorun ("Dcell", N_name);
		    flush_keyboard ();
		    fprintf (stderr, "Press enter to continue. ");
		    {
			char buf[1024];
			gets (buf);
		    }
		    myrespend ();
		    R_open_driver ();
		    break;
#endif
		/*
		case MTC_DISPLAY:
		    break;
		*/
		case MTC_DUPLICATE:
		    display_duplicate_lines (CMap);
		    break;
		case MTC_GARBAGE:
		    _Clear_info ();
		    compress (CMap, 1);
		    break;
		case MTC_NEAT:
		    build_neat (CMap);
		    break;
		case MTC_MEMORY:
		    break;
		case MTC_WRITE:
		    _Clear_info ();
		    if ( 0 > write_out (1))
		    {
			BEEP;
			Write_info (2, "Write FAILED!!!");
			sleep (4);
		    }
		    else
		    {
			Changes_Made = 0;
			Write_info (2, "File updated...");
			sleep (2);
		    }
		    break;
		case MTC_REGIST:
	/*  NEED to check out this code */
		    if (do_graphics())
		    {
			reset_map(CMap, CMap->coor_file);

			set_window_w();
			R_standard_color( dcolors[CLR_ERASE]);
			erase_window();
			outline_window();
			replot(CMap);
		    }
		    break;
		case MTC_SHELL:
#ifdef CERL
		    mysuspend ();
		    R_close_driver ();
		    /* swap_re_uids ();	    */
		    if (fork () == 0)
		    {
			/* reset back to REAL user */
			if (0 > set_uid_to_user ())
			    _exit (127);
			fprintf (stderr, "\nType 'exit' to return\n");
			execl ("/bin/csh", "csh", 0);
			_exit (127);
		    }
		    else
		    {
			wait (&stat);
		    }
		    R_open_driver ();
		    /* swap_re_uids ();	    */
		    myrespend ();
#endif
		    break;
		case MTC_ULAREAS:
		    display_unlabeled_areas (CMap);
		    break;
		case MTC_BADAREAS:
		    unfinished_areas (CMap);
		    break;
		case MTC_NODELINES:
		    node_lines (CMap);
		    break;
		case MTC_ISLES:
		    display_islands (CMap);
		    break;
		case MTC_QUIT:
		    Pass = 0;
		    goto TOOL_END;
		    break;
		default:
		    break;
	    }
	}
	else
	{
	    if ((ret = global_menu (chr, &M_tool)) > 0)
	    {
		Pass = ret;	/* should never get here for window () */
		break;  /* return and execute new command */
	    }
	    if (ret < 0)
		BEEP;
	}
    }
TOOL_END:
    Set_Global (MG_DIGIT, ON);
    Set_Global (MG_EDIT, ON);
    Set_Global (MG_LABEL, ON);
    Set_Global (MG_CUSTOM, ON);
    Set_Global (MG_TOOL, ON);
    Set_Global (MG_QUIT, ON);
    return (Pass);
}
