/* Function: Cont_rel		P.W. Carlson		5/89	*/

#include "ega_io.h"

Cont_rel(delta_x, delta_y)
int delta_x, delta_y;
{
    /* update the current coordinates */
    cur_x += delta_x;
    cur_y += delta_y;

    /* pass the data to the device driver */
    args.arg1 = cur_x;
    args.arg2 = cur_y;
    ioctl(egafd, EGA_DITHDRAW, &args);
}
