#include "globals.h"

static int use = 1;

set_colors (colors)
    struct Colors *colors;
{
    D_set_colors (colors);
}


get_vector_color()
{
    int setmap_blue();   
    int setmap_gray();
    int setmap_green();
    int setmap_red();
    int setmap_white();
    int setmap_yellow();
    int x, y;
	
    static Objects objects[] =
    {
 	INFO("Pick color for vectors ->", &use),
	MENU("BLUE", setmap_blue, &use),
	MENU("GRAY", setmap_gray, &use),
	MENU("GREEN", setmap_green, &use),
	MENU("RED", setmap_red, &use),
	MENU("WHITE", setmap_white, &use),
	MENU("YELLOW", setmap_yellow, &use),
	{0} 
    };

    x = (SCREEN_LEFT + SCREEN_RIGHT) / 2;
    y = SCREEN_BOTTOM;
    Set_mouse_xy( x,y );

    Input_pointer(objects);
    return 0;  /* return but don't quit */
}

static
setmap_blue()
{
	line_color = COLOR_BLUE;
}

static
setmap_gray()
{
	line_color = COLOR_GREY;
}

static
setmap_green()
{
	line_color = COLOR_GREEN;
}

static 
setmap_red()
{
	line_color = COLOR_RED;
}

static
setmap_white()
{
	line_color = COLOR_WHITE;
}

static 
setmap_yellow()
{
	line_color = COLOR_YELLOW;
}

static int done()
{
	return -1;
} 
