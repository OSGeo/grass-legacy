#include <grass/display.h>

int move (int x, int y)
{
    return 0;
}

int cont (int x, int y)
{
    return 0;
}

int setup_graphics (void)
{
    D_setup(0);
    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	move, cont);

    return 0;
}
