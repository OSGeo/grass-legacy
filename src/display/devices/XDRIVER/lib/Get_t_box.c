
#include "driver.h"
Get_text_box(text, t, b, l, r)
char *text;
int *t, *b, *l, *r;
{
    soft_text_ext(cur_x, cur_y,
            _text_size_x, _text_size_y, _text_rotation, text);
    get_text_ext(t, b, l, r);
}
