#include <stdio.h>

int Ppictsize (int rows, int cols)
{
    char buf[128];

/* P6 is code for ppm format */
    Pouts("P6\n");
/* next line is width height */
    sprintf (buf, "%d %d\n", cols, rows);
    Pouts(buf);
/* maximum color level. */
    Pouts("255\n");

    return 0;
}
