#include "P.h"
#include "raster.h"

int send_data (int n)
{
    R_move_abs (left_edge, current_row++);
    R_raster (n, 1, 1, data);

    return 0;
}

int Pdata (unsigned char *buf,int n) 
{
    int *d;
    int i;

    for (d = data, i = 0; i < n; i++)
	*d++ = *buf++;
    send_data (n);

    return 0;
}
