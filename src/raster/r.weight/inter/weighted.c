
#include "include.h"
#include "display.h"
#include "D.h"
#include <unistd.h>
#include "raster.h"

struct mapdef mapdef[MAX_MAPS] ;
int analysis_type = ADD ;
long normalize = 0 ;
char map_name[20] ;
char input_buf[256] ;

static int is_console ;

int 
main (int argc, char **argv)
{
    int i, outcome ;
    int offset ;
    char window_name[64] ;

    G_gisinit(argv[0]) ;

/* Advertising */
    if (isatty(0))
	G_gishelp("WEIGHT", "intro") ;

    if(G_yes("Do you want graphics to go to the color monitor? ", 0))
	is_console = 1 ;
    else
	is_console = 0 ;

/* Initialize graphics if at console */
    if ( is_console )
    {
	R_open_driver() ;

	if (D_get_cur_wind(window_name))
	{
	    Dclearscreen();
	    Dnew ("full", 0, 100, 0, 100);
	    Dchoose ("full");
	}

	if (D_get_cur_wind(window_name))
	    G_fatal_error("No current graphics window") ;

	if (D_set_cur_wind(window_name))
	    G_fatal_error("Current graphics window not available") ;

	D_set_cell_name("weight result") ;

/* Get color offset value for current graphics window and pass to driver */
	D_offset_is(&offset) ;
	R_color_offset(offset) ;
    }


    fprintf (stdout,"For help, enter: help\n\n") ;

    for (i=0; i < MAX_MAPS; i++)
	mapdef[i].used = 0;

    while (1)
    {
	fprintf (stdout,"weight: ") ;

	outcome = yyparse() ;

	switch(outcome)
	{
	    case QUIT_EXPR:
		exit(0) ;
		break ;
	    default:
		break ;
	}
    }
}

int 
at_console (void)
{
    return(is_console) ;
}
