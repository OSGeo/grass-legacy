/*  @(#)graphics.c    2.1  6/26/87  */
#include "graphics.h"

static int dographics;

init_graphics()
{
    int	top,    bot,    left,    right;

    /* find out if we want to use the color monitor */

    /***       DEBUG
    if (curses_yes_no(1, "Do you want color monitor graphics? > ") )
	dographics = 1;
    else
	dographics = 0; 
    ***/
    dographics = 1;

    if(dographics)
    {
	R_open_driver();
	D_get_screen_window ( &top, &bot, &left, &right);
	screen_left  = left;
	screen_right = right;
	screen_bot   = bot;
	screen_top   = top;
	R_font ("romans");
	R_text_size (12, 12);
    }
}

do_graphics()
{
    return(dographics);
}
