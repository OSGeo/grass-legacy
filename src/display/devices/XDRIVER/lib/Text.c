
#include "driver.h"
Text(text)
char *text;
{
    soft_text(cur_x, cur_y,
            _text_size_x, _text_size_y, _text_rotation, text);
}
