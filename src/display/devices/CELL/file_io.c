

/*
**  code to write data to tmp file 
*/
#include "cell.h"

extern int screen_left   ;
extern int screen_right  ;
extern int screen_bottom ;
extern int screen_top    ;

int store_xy (int x, int y)
{
    int width = screen_right - screen_left;

    if ( x < screen_left || x >= screen_right ||
	 y < screen_top  || y >= screen_bottom )
	    return 0;

/*DEBUG fprintf (stderr, "STORE_XY  (%d,%d)  %d\n", x, y, Cur_color); */
    fseek (Temp_fp, y * width + x, 0);
    fwrite (&Cur_color, 1, 1,  Temp_fp);

    return 0;
}

int horiz_line (int y, int x1, int x2)
{
    int width = screen_right - screen_left;
    int i, len;

    if (( y  <  screen_top   || y  >= screen_bottom ) ||
	( x1 <  screen_left  && x2 <  screen_left   ) ||
	( x1 >= screen_right && x2 >= screen_right  ))
	    return 0;

    if ( x1 > x2 ) {
	int tmp = x1;
	x1 = x2;
	x2 = tmp;
    }

    x1 = (x1 < screen_left  ? screen_left  : x1);
    x2 = (x2 > screen_right ? screen_right : x2);

    len = x2 - x1;
    for (i = 0 ; i < len ; i++)
	Row_buf[i] = Cur_color;

    fseek (Temp_fp, (long) (y - screen_top) * width + x1, 0);
    fwrite (Row_buf, 1, len,  Temp_fp);
    return (0);
}
