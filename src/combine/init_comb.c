/* init_comb handles all start up tasks associated with combinate */

#include "tree.h"
#include "colors.h"

static int is_console ;

init_comb()
{
    int offset ;
    char window_name[64] ;

/* find out if we are at the console */

    is_console = G_yes("Do you want graphics to go to the color monitor? ", 0) ;

    if(at_console())
    {
	R_open_driver() ;

	if (D_get_cur_wind(window_name))
	{
	    Dclearscreen();
	    Dnew ("full",0,100,0,100);
	    Dchoose("full");
	}
	if (D_get_cur_wind(window_name))
	    G_fatal_error("No current graphics window") ;

	if (D_set_cur_wind(window_name))
	    G_fatal_error("Current graphics window not available") ;

	D_set_cell_name("combine result") ;

/* Get color offset value for current graphics window and pass to driver */
	D_offset_is(&offset) ;
	R_color_offset(offset) ;

	R_reset_colors(0, NUM_COLORS, red, grn, blu) ;
    }

    /* initialize the mapset window */
    if(init_proj_win() == 0)
	yyerror("Can't initialize given mapset") ;

    if(at_console())
    {
	printf("\nAnalyses will be displayed in color during execution\n");
    }
    else
    {
	printf("\nAnalyses will be displayed with symbols during execution\n");
    }

    /* Letem know help is available */
    printf("\nHelp is available for combine: enter (help)\n") ;
}

at_console()
{
    return(is_console) ;
}
