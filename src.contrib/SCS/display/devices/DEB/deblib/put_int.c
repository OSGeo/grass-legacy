

/*---------- Function: put_int ----------*/
#include <stdio.h>
put_int(num)
int num;
{
    unsigned char c1, c2;

    c1 = (num & 0x1f) | 0x20;  
    putc(c1, stdout);
    c2 = ((num & 0x03e0) >> 5) | 0x20;
    putc(c2, stdout);
    fflush(stdout);
}
