

/*---------- Function: set_window ----------*/
extern int wind_x1, wind_y1, wind_x2, wind_y2;
set_window(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
    put_chr('V');
    put_int(x1);
    put_int(y1);
    put_int(x2);
    put_int(y2);
    wind_x1 = x1;
    wind_y1 = y1;
    wind_x2 = x2;
    wind_y2 = y2;
}
