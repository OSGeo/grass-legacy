

/*---------- Function: enter_gmode ----------*/
#include <stdio.h>
#include <termio.h>
extern int wind_x1, wind_y1, wind_x2, wind_y2;
enter_gmode()
{
    struct termio tty_param, tty_in;
    int out, in;

    wind_x1 = wind_y1 = 0;
    wind_x2 = 639;
    wind_y2 = 399;
}
