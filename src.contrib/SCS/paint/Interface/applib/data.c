#include "interface.h"

Pdata_begin()
{
    P__opcode (DATA);
}

Pdata (buf, n)
    char *buf;
{
    P__send (buf, n);
}

Pdata_end()
{
}
