#include "tape.h"

header(buf)
    unsigned char buf[] ;
{
    nbands = buf[120];
    bandfd = (int *) G_malloc (nbands * sizeof(int));
}
