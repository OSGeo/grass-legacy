#include "globals.h"

static int use = 1;

set_colors (colors) 
    struct Colors *colors;
{
    D_set_colors (colors);
}

set_menu_colors (colors)
    struct Colors *colors;
{

/* SCREEN OUTLINE and CURSOR */
    G_add_color_rule((CELL)241,180, 180, 180, (CELL)241,180, 180, 180, colors);
/* RED */
    G_add_color_rule((CELL)242,200,  90,  90, (CELL)242,200,  90,  90, colors);
/* ORANGE */
    G_add_color_rule((CELL)243,150, 100,  50, (CELL)243,150, 100,  50, colors);
/* YELLOW */
    G_add_color_rule((CELL)244,200, 200,  10, (CELL)244,200, 200,  10, colors);
/* GREEN */
    G_add_color_rule((CELL)245, 90, 200,  90, (CELL)245, 90, 200,  90, colors);
/* BLUE */
    G_add_color_rule((CELL)246, 90,  90, 200, (CELL)246, 90,  90, 200, colors);
/* INDIGO */
    G_add_color_rule((CELL)247,100, 100,  10, (CELL)247,100, 100,  10, colors);
/* VIOLET */
    G_add_color_rule((CELL)248,150, 150,  10, (CELL)248,150, 150,  10, colors);
/* WHITE */
    G_add_color_rule((CELL)249,250, 250, 250, (CELL)249,250, 250, 250, colors);
/* BLACK */
    G_add_color_rule((CELL)250,  0,   0,   0, (CELL)250,  0,   0,   0, colors);
/* GRAY */
    G_add_color_rule((CELL)251,180, 180, 180, (CELL)251,180, 180, 180, colors);
/* BROWN */
    G_add_color_rule((CELL)252,100, 100,  30, (CELL)252,100, 100,  30, colors);
/* MAGENTA */
    G_add_color_rule((CELL)253,150,  90, 150, (CELL)253,150,  90, 150, colors);
/* AQUA */
    G_add_color_rule((CELL)254, 50, 120, 120, (CELL)254, 50, 120, 120, colors); 
/*      */
    G_add_color_rule((CELL)255,250,   0,   0, (CELL)255,250,   0,   0, colors); 

    set_colors (colors);
}

cursor_color()
{
    int done();
    int set_blue();   
    int set_gray();
    int set_green();
    int set_red();
    int set_white();
    int set_yellow();
	
    static Objects objects[] =
    {
	MENU("DONE", done, &use),
 	INFO("Pick a Color ->", &use),
	MENU("BLUE", set_blue, &use),
	MENU("GRAY", set_gray, &use),
	MENU("GREEN", set_green, &use),
	MENU("RED", set_red, &use),
	MENU("WHITE", set_white, &use),
	MENU("YELLOW", set_yellow, &use),
	{0} 
    };

    while( (Input_pointer(objects) != -1) );
    return 0;  /* return but don't quit */
}

static
set_blue()
{
	set_cur_clr(BLUE);
	return 0;
}

static
set_gray()
{
	set_cur_clr(GREY);
	return 0;
}

static
set_green()
{
	set_cur_clr(GREEN);
	return 0;
}

static
set_red()
{
	set_cur_clr(RED);
	return 0;
}

static
set_white()
{
	set_cur_clr(WHITE);
	return 0;
}

static
set_yellow()
{
	set_cur_clr(YELLOW);
	return 0;
}
static int set_cur_clr( curs_color)
{
struct Colors *colors;

colors = &VIEW_MAP1->cell.colors;
    
    switch(curs_color)
	{
	case 5:    /* BLUE */

    G_add_color_rule((CELL)241, 90,  90, 200, (CELL)241, 90,  90, 200, colors);
	break;

	case 10:   /* GRAY */

    G_add_color_rule((CELL)241,180, 180, 180, (CELL)241,180, 180, 180, colors);
	break;

	case 4:    /* GREEN */

    G_add_color_rule((CELL)241, 90, 200,  90, (CELL)241, 90, 200,  90, colors);
	break;

	case 1:    /* RED */

    G_add_color_rule((CELL)241,200,  90,  90, (CELL)241,200,  90,  90, colors);
	break;

	case 8:    /* WHITE */

    G_add_color_rule((CELL)241,250, 250, 250, (CELL)241,250, 250, 250, colors);
	break;

	case 3:   /* YELLOW */

    G_add_color_rule((CELL)241,200, 200,  10, (CELL)241,200, 200,  10, colors);
	break;
    }

    set_colors (colors);
    return 0;
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
	line_color = BLUE;
}

static
setmap_gray()
{
	line_color = GREY;
}

static
setmap_green()
{
	line_color = GREEN;
}

static 
setmap_red()
{
	line_color = RED;
}

static
setmap_white()
{
	line_color = WHITE;
}

static 
setmap_yellow()
{
	line_color = YELLOW;
}

static int done()
{
	return -1;
} 
