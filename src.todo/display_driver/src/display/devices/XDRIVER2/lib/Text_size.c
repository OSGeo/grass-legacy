#include "driver.h"

int Text_size (int x, int y)
{
    _text_size_x = (double) x / 25.0;
    _text_size_y = (double) y / 25.0;
    return 0;
}

int Text_rotation (double val)
{
    return _text_rotation = (double) val;
}
