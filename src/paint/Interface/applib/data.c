#include "interface.h"

int Pdata_begin (void)
{
    P__opcode (DATA);

    return 0;
}

int Pdata (unsigned char *buf, int n)
{
    P__send (buf, n);

    return 0;
}

int Pdata_end (void)
{
    return 0;
}
