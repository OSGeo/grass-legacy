

/*---------- Function: clear_window ----------*/
extern int wind_x1, wind_y1, wind_x2, wind_y2;
clear_window()
{
    put_chr('E');
    put_int(wind_x1);
    put_int(wind_y1);
    put_int(wind_x2);
    put_int(wind_y2);
}
