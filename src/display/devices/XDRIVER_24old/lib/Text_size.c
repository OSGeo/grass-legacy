#include "driver.h"

Text_size(x, y)
int x, y;
{
    _text_size_x = (double) x / 25.0;
    _text_size_y = (double) y / 25.0;
}

Text_rotation(val)
float val;
{
    _text_rotation = (double) val;
}
