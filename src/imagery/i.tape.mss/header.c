#include "tape.h"

int 
header (unsigned char buf[])
{
    nbands = buf[120];
    bandfd = (int *) G_malloc (nbands * sizeof(int));

    return 0;
}
