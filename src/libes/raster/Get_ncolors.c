#include "graph.h"

R_get_num_colors (n)
    int *n;
{
    _send_ident(GET_NUM_COLORS) ;
    _get_int(n) ;
}

