#include "raster.h"
#include "graph.h"

int R_get_num_colors (int *n)
{
    _send_ident(GET_NUM_COLORS) ;
    _get_int(n) ;

	return 0;
}
