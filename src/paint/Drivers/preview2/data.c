#include "P.h"
#include "raster.h"
#include "Paintlib.h"
#include "local_proto.h"

int Pdata (unsigned char *buf, int n)
{
    int *d;
    int i;

    for (d = data, i = 0; i < n; i++)
	*d++ = *buf++;
    send_data (n);

    return 0;
}

int send_data (int n)
{
    if (current_row == window_nrows) erase();
    current_row++;
    R_move_abs (left_edge, top_edge++);
    R_raster (n, 1, 1, data);

    return 0;
}
