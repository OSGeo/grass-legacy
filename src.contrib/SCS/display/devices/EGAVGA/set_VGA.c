/* Function: Graph_Set (VGA)   	P.W. Carlson		April 1990	*/

#include "driver.h"

int SCREEN_LEFT;
int SCREEN_RIGHT;
int SCREEN_BOTTOM;
int SCREEN_TOP;
int NCOLORS;
int cur_color;

Graph_Set() 
{
    /* put emulator tty name in environment for use with DOS_list */
    G__setenv("DEBTTY", ttyname(0));
    G__write_env();

    SCREEN_LEFT = 0;
    SCREEN_RIGHT = 639;
    SCREEN_BOTTOM = 479;
    SCREEN_TOP = 0;
    NCOLORS = 125;

    cur_x = 0;
    cur_y = 0;

    enter_gmode(0x12);

    put_chr('h');
}
