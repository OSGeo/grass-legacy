#include "../XDRIVER.h"
#include "driver.h"

int Get_text_box (char *text, int *t, int *b, int *l, int *r)
{
    soft_text_ext(cur_x, cur_y,
            _text_size_x, _text_size_y, _text_rotation, text);
    get_text_ext(t, b, l, r);

    return 0;
}
