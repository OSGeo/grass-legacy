

/*
**  code to write data to tmp file 
*/
#include "cell.h"

int put_row(int y, int x1, int x2, const unsigned char *data)
{
    int width = screen_right - screen_left;
    fseek(Temp_fp, (long) (y - screen_top) * width + x1, SEEK_SET);
    fwrite(data, 1, x2 - x1, Temp_fp);
    return 0;
}

int get_row(int y, int x1, int x2, unsigned char *data)
{
    int width = screen_right - screen_left;
    fseek(Temp_fp, (long) (y - screen_top) * width + x1, SEEK_SET);
    fread(data, 1, x2 - x1, Temp_fp);
    return 0;
}

int store_xy (int x, int y)
{
    int width = screen_right - screen_left;

    if ( x < screen_left || x >= screen_right ||
	 y < screen_top  || y >= screen_bottom )
	    return 0;

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

    put_row(y, x1, x2, Row_buf);

    return 0;
}

