#include "globals.h"
#include "display.h"

static int use = 1;
static int done(void);
static int setmap_blue(void);
static int setmap_gray(void);
static int setmap_green(void);
static int setmap_white(void);
static int setmap_red(void);
static int setmap_yellow(void);

int set_colors (struct Colors *colors)
{
    D_set_colors (colors);

    return 0;
}


int get_vector_color()
{
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

static int setmap_blue()
{
	line_color = I_COLOR_BLUE;

    return 0;
}

static int setmap_gray()
{
	line_color = I_COLOR_GREY;

    return 0;
}

static int setmap_green()
{
	line_color = I_COLOR_GREEN;

    return 0;
}

static int setmap_red()
{
	line_color = I_COLOR_RED;

    return 0;
}

static int setmap_white()
{
	line_color = I_COLOR_WHITE;

    return 0;
}

static int setmap_yellow()
{
	line_color = I_COLOR_YELLOW;

    return 0;
}

static int done()
{
	return -1;
} 
