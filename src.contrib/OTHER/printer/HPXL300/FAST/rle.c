#include "P.h"
#include <malloc.h>

unsigned char bytes[5000];


Prle (buf, n)
    unsigned char *buf;
{
    int i;

    for (i = 0; i < n*2; i+=2)  {
        buf[i] = ((buf[i] - 1) < 0) ? 255: buf[i] - 1;
     } 

    sprintf(buffer,"*b%dW",n*2);
    esc(buffer);
    Pout(buf,n*2);
}
