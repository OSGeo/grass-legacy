/*  @(#)graphics.c    2.1  6/26/87  */
/**  Re-Written by Ron Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/
#include "graphics.h"
#include "structs.h"
#include <stdio.h>

static int dographics;

init_graphics()
{
    int	top,    bot,    left,    right;

    /* find out if we want to use the color monitor */

    /***       DEBUG
    if (G_yes(1, "Do you want color monitor graphics? > ") )
	dographics = 1;
    else
	dographics = 0; 
    ***/
    dographics = 1;

    if(dographics)
    {
	R_open_driver();
/* Make sure screen is clear */
	Dclearscreen() ;

/* Establish windows on screen */
  	Dnew(DIG.name, DIG.bot, DIG.top, DIG.left, DIG.right) ;
  	Dnew(MEN.name, MEN.bot, MEN.top, MEN.left, MEN.right) ; 
	Dchoose(DIG.name) ;
        D_get_screen_window ( &top, &bot, &left, &right);
	screen_left  = left;
	screen_right = right;
	screen_bot   = bot;
	screen_top   = top;
    }
}

do_graphics()
{
    return(dographics);
}
