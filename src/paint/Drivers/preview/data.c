#include "P.h"

Pdata (buf, n) 
    unsigned char *buf;
{
    int *d;
    int i;

    for (d = data, i = 0; i < n; i++)
	*d++ = *buf++;
    send_data (n);
}
send_data (n)
{
    R_move_abs (left_edge, current_row++);
    R_raster (n, 1, 1, data);
}
