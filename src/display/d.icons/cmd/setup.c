#include "gis.h"
#include "display.h"

int 
setup_plot (void)
{
    D_setup(0);

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);

    return 0;
}
