#include "ginput.h"

int
get_diginput (x, y)
    int *x, *y;
{
    char buf[10];
    int button;

    if ((button = dig_input(x, y)) < 0)
	D_ask_if_err();

    return button;
}
