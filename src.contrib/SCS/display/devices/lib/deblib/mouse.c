
#include <stdio.h>
extern int mouse_x, mouse_y;

/*---------- Function: on_mouse ----------*/
on_mouse()
{
    put_chr('O');
    unblock_mode();
}

/*---------- Function: off_mouse ----------*/
off_mouse()
{
    put_chr('X');
    normal_mode();
}

/*---------- Function: mouse_curs_on -----------*/
mouse_curs_on()
{
    put_chr('o');
}

/*---------- Function: mouse_curs_off -----------*/
mouse_curs_off()
{
    put_chr('x');
}

/*---------- Function: get_mouse_but ----------*/
char get_mouse_but()
{
    char c;
    put_chr('B');
    c = getchar();
    mouse_x = get_int();
    mouse_y = get_int();
    return(c);
}

/*---------- Function: mouse_area ----------*/
mouse_area()
{
    put_chr('A');
}
