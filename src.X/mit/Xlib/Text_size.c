/* %W% %G% */

extern double _text_size_x, _text_size_y, _text_rotation;

Text_size(x, y)
int x, y;
{
/*
    printf("\n size_x = %d, size_y = %d", x, y);
*/

    _text_size_x = ((double) x) / 25.0;
    _text_size_y = ((double) y) / 25.0;
/*
    printf("\n text_s = %lf, text_y = %lf",
        _text_size_x, _text_size_y);
*/
}

Text_rotation(val)
float val;
{
    _text_rotation = (double) val;
}
